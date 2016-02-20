#! /opt/local/bin/Rscript

library("data.table")
library("ggplot2")
library("ggthemes")
library("gridExtra")

CWD <- "~/Coursera/DataScience/Part4/DataScience_Part4_Assignment2"  # working directory
setwd(CWD)

# function to get zip archive from web and save it in working directory 
# should be run only once return date and time data was downloaded 
get_data_from_web <- function(URL = character()){
    download.file(url = URL, destfile = "./Emissions_Data.zip", 
                  quiet = T, method="curl")
    dateDownloaded <- date()
    unzip(zipfile = "./Emissions_Data.zip")
    #file.rename(from = "./UCI HAR Dataset/", to = PATH) #remove spaces in dir name
    return(dateDownloaded)
}

cacheData <- function(x, ...) {
    D.F <- x$get_df()
    if(!is.null(D.F)) {
        #    message("getting cached data")
        return(D.F)
    }
    data <- x$get()
    D.F <- get_data_helper(File = data,  ...)
    x$set_df(D.F)
    return(D.F)
}

read_dataCache <- function(file = character()) {
    D.F <- NULL
    set <- function(y) {
        file <<- y
        D.F <<- NULL
    }
    get <- function() {
        file
    }
    set_df <- function(df) {
        D.F <<- df
    }
    get_df <- function() {
        D.F
    }
    return (list (set = set, get = get, 
                  set_df = set_df, get_df = get_df))
}

get_data_helper <- function(File = character()) {
    if (file.exists(File) ){
        return ( data.table(readRDS(File)) )
    }
    URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" # url to data
    dataDate <- get_data_from_web(URL)
    print( sptrintf("Data Set was downloaded on %s\n", dataDate))
    return ( data.table(readRDS(File)) )
}

MyTheme <- theme_stata()

FileEmissionSummary <- "./summarySCC_PM25.rds" 
FileSource <- "./Source_Classification_Code.rds"

DataCacheSummary <- read_dataCache(FileEmissionSummary)
NEI <- cacheData(DataCacheSummary)

DataCacheSoure <- read_dataCache(FileSource)
SCC <- cacheData(DataCacheSoure)

# LA vs BC
setkey(SCC, EI.Sector)
setkey(NEI, fips)
Vehicles.SCC.list <- droplevels(SCC[grep(EI.Sector, pattern = "Vehicles"), SCC])
NEI.Vehicles.BCLA <- subset(x = NEI, SCC %in% Vehicles.SCC.list & fips %in% list("24510","06037") )
setkey(NEI.Vehicles.BCLA, fips)
Vehicles.BCLA.T <- NEI.Vehicles.BCLA[, sum(Emissions), by=list(year, fips)]
setkey(Vehicles.BCLA.T, year)
changeLA <- Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="06037"]$V1 - 
Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="06037" & Vehicles.BCLA.T$year==1999 ]$V1
changeBC <- Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="24510"]$V1 - 
Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="24510" & Vehicles.BCLA.T$year==1999 ]$V1

png("./plot6.png", 
    width = 800, height = 1200, units = "px", pointsize = 12)
par(mfrow=c(2,1))
plot(Vehicles.BCLA.T$year, Vehicles.BCLA.T$V1, type='n', ylim=c(100, 10000),
     ylab="PM2.5 emmission [tons]", xlab="Years" , log="y", yaxt="n", 
     main="PM2.5 emission from vehicle sources")
axis(2, at = c(100, 1000, 10000) , las=0)
lines(Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="06037"]$year, 
      Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="06037"]$V1, type="o", lwd=2, pch=16,  col="red")
lines(Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="24510"]$year, 
      Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="24510"]$V1, type="o",  lwd=2, pch=16, col="blue")
legend("top", col=c( "red","blue"), 
       legend=c("LA", "BC"), horiz=T,
       lwd=2)

plot(Vehicles.BCLA.T$year, type='n', xlim = c(1999, 2008),
     ylim=c(min(c(changeBC, changeLA)), max(c(changeBC, changeLA))),
     ylab="PM2.5 emmission [tons]", xlab="Years" ,  
     main="Change of PM2.5 emission from vehicle sources")
lines(Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="06037"]$year, changeLA, 
      type="s", lwd=2, pch=16,  col="red")
lines(Vehicles.BCLA.T[Vehicles.BCLA.T$fips=="24510"]$year, changeBC, 
      type="s",  lwd=2, pch=16, col="blue")
legend("topleft", col=c( "red","blue"), 
       legend=c("LA", "BC"), horiz=T,
       lwd=2)
dev.off()

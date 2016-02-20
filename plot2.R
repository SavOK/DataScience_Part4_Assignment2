#! /opt/local/bin/Rscript

library("data.table")
library("ggplot2")
library("ggthemes")
library("gridExtra")

CWD <- "~/Coursera/DataScience/Part4/DataScience_Part4_Assignment2"  # working directory
# URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" # url to data
#PATH <- "./UCI_HAR_Datase" # relative path to the data
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
# MyTheme <- MyTheme + theme(axis.title.x = element_text(size=14, vjust=0),
#                           axis.title.y = element_text(size=14, vjust=-100), 
#                           legend.title = element_text(size=12)
# )

FileEmissionSummary <- "./summarySCC_PM25.rds" 
FileSource <- "./Source_Classification_Code.rds"

DataCacheSummary <- read_dataCache(FileEmissionSummary)
NEI <- cacheData(DataCacheSummary)

DataCacheSoure <- read_dataCache(FileSource)
SCC <- cacheData(DataCacheSoure)

# Baltimore City, Maryland total emission
setkey(NEI, fips)
BC.Total.Polution <- NEI[i=list("24510"), j=sum(Emissions), by=year]
colnames(BC.Total.Polution) <- c("year", "TotalEmission")

png("./plot2.png", 
    width = 800, height = 600, units = "px", pointsize = 12)
plot(x=BC.Total.Polution$year, y=BC.Total.Polution$TotalEmission, type = "o", 
     main = "Total number of PM2.5 emission in Baltimore City", col = "red", lwd=2, pch=16, 
     xlab = "year", ylab = "Total PM2.5 emisson [tons]")
dev.off()

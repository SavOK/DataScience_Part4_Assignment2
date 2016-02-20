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

FileEmissionSummary <- "./summarySCC_PM25.rds" 
FileSource <- "./Source_Classification_Code.rds"

DataCacheSummary <- read_dataCache(FileEmissionSummary)
NEI <- cacheData(DataCacheSummary)

DataCacheSoure <- read_dataCache(FileSource)
SCC <- cacheData(DataCacheSoure)

# Baltimore City Maryland
setkey(NEI, fips)
BC.Polution <- NEI["24510", sum(Emissions), by=list(type, year)]

Plot.BC <- ggplot(BC.Polution, aes(x=year, y=V1))
Plot.BC <- Plot.BC + geom_line(aes(color=type), size=1.5)
Plot.BC <- Plot.BC + scale_y_continuous(name="PM2.5 emmission [tons]")
Plot.BC <- Plot.BC + scale_x_continuous(name="Years")
Plot.BC <- Plot.BC + scale_color_discrete(name="Source type")
Plot.BC <- Plot.BC + ggtitle("PM2.5 emission in Baltimore City from deferent source types")

png("./plot3.png", 
    width = 800, height = 600, units = "px", pointsize = 12)
grid.arrange ( Plot.BC + theme_stata(), ncol=1)
dev.off()


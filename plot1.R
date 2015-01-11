## Author: Brian McNally
## Version: 1.0
## Date: 2015.01.19
## This R Module creates a plot, saved as plot1.png in the working directory,
## for Project 1.  A lot of things are hardcoded ... which is kind of bad form.
## ToDO: Improve code for data frame.  Works but not as efficient.

plot1 <- function()
{
  data <- getDataForGraph()
  
  ## Opens output device and sets dimensions
  png(filename = "plot1.png", width = 480, height = 480, units = "px")
  
  ## Graphs red columned histogram to png output device
  hist(data$Global_active_power, 
       col="red", 
       xlab="Global active power (kilowatts)", 
       main="Global Active Power")
  
  
  dev.off()
  return("Plot1 printed to plot1.png")
}

## Helper function for data load. Replicated w/out change across plot.R files
## Returns data table of complete rows within time frame

getDataForGraph <- function()
{  
  data <- read.table("household_power_consumption.txt", sep=";", 
                     colClasses = c("character", "character", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"), 
                     header = TRUE, na.strings = "?") 
  
  
  data$DateTime <- as.POSIXct(paste(data$Date, data$Time), 
                              format="%d/%m/%Y %H:%M:%S")
  
  ## Clean out bad rows and subset to only the needed dates
  ## Per Requirements - only looks at data from 02/01/2007 - 02/02/2007
  
  data <- data[complete.cases(data) & 
                 (data$DateTime >= as.POSIXct("2007-02-01") 
                  & data$DateTime < as.POSIXct("2007-02-03")),]
  
  return(data)
}

## Author: Brian McNally
## Version: 1.0
## Date: 2015.01.19
## This R Module creates a plot, saved as plot3.png in the working directory,
## for Project 1.  A lot of things are hardcoded ... which is kind of bad form.
## ToDO: Improve code for data frame.  Works but not as efficient.

plot3 <- function()
{
  data <- getDataForGraph()
  
  ## Opens output device and sets dimensions
  png(filename = "plot3.png", width = 480, height = 480, units = "px")
  
  ## Plots X-Y lines in different colors for Energy Sub metering
  ## against DateTime
  plot(Sub_metering_1 ~ DateTime, 
       data = data, 
       type="l", 
       ylab="Energy sub metering", xlab="")
  lines(data$DateTime, data$Sub_metering_2, type="l", col="red")
  lines(data$DateTime, data$Sub_metering_3, type="l", col="blue")
  
  legend("topright", lty=c(1,1,1), col=c("black", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  dev.off()
  return("Plot3 printed to plot3.png")
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

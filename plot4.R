## Author: Brian McNally
## Version: 1.0
## Date: 2015.01.19
## This R Module creates a plot, saved as plot4.png in the working directory,
## for Project 1.  A lot of things are hardcoded ... which is kind of bad form.
## ToDO: Improve code for data frame.  Works but not as efficient.

plot4 <- function()
{
  data <- getDataForGraph()
  
  ## Opens output device and sets dimensions
  png(filename = "plot4.png", width = 480, height = 480, units = "px")
  
  ## Sets 2x2 grid of graphs
  par(mfrow = c(2,2))
  ## Plots each graph into quadrant going from L-R, U-D
  with(data, {
    ## Plots Global Active Power vs DateTime in XY Line graph
    plot(data$DateTime, data$Global_active_power, type="l", 
         xlab="", ylab="Global Active Power")
    
    # Plots Voltage vs Datetime in XY Line graph
    plot(data$DateTime, data$Voltage, type="l", 
         xlab="datetime", ylab="Voltage")
    
    ## Plots Energy Sub Metering vs DateTime, Different colors
    plot(Sub_metering_1 ~ DateTime, data = data, type="l", 
         ylab="Energy sub metering", xlab="")    
    lines(data$DateTime, data$Sub_metering_2, type="l", col="red")
    lines(data$DateTime, data$Sub_metering_3, type="l", col="blue") 
    legend("topright", bty="n", lty=c(1,1,1), col=c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    ## Plots Global Reactive Power vs Date Time in XY Line graph
    plot(data$DateTime, data$Global_reactive_power, type="l", 
         xlab="datetime", ylab="Global_reactive_power")
  })
  dev.off()
  return("Plot4 printed to plot4.png")
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

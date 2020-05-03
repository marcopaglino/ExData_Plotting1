# plot4.R
# The function draw a plot based on the 
# measurements of electric power consumption in one household 
# from 2007-02-01 to 2007-02-02.
# The graph displays 4 plots:
# Global Active Power, Voltage, Energy sub metering, Global reactive power
# 
plot4 <- function() {
        # load the dplyr library to manipulate data
        library(dplyr)
        # read raw data in a data.frame, extracting only the data from
        # the dates 2007-02-01 and 2007-02-02 and converting to numeric the  variables:
        # Sub_metering_1, Sub_metering_2, Sub_metering_3, Global_reactive_powe and Voltage
        powercons <- read.csv2("data/household_power_consumption.txt", stringsAsFactors = F,
                               na.strings = "?") %>% 
                subset(Date=="1/2/2007" | Date=="2/2/2007") %>% 
                mutate(Sub_metering_1 = as.numeric(Sub_metering_1)) %>%
                mutate(Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
                mutate(Sub_metering_3 = as.numeric(Sub_metering_3)) %>%
                mutate(Global_reactive_power = as.numeric(Global_reactive_power)) %>%
                mutate(Voltage = as.numeric(Voltage))
        
        # converting Date and Time
        powercons$Time <- strptime(paste(powercons$Date,powercons$Time),"%d/%m/%Y %H:%M:%S")
        powercons$Date <- as.Date(powercons$Date,"%d/%m/%Y")
        
        # open the bitmap device
        png(file="plot4.png")
        
        # define the layout for the plots: 2 rows and 2 columns
        par(mfrow = c(2,2))
        # set the locale to english
        Sys.setlocale("LC_TIME", locale = "English")
        
        # draw the graph for Global Active Power
        with(powercons,plot(Time,Global_active_power, 
                            type="l", xlab="",
                            ylab="Global Active Power"))
        
        # draw the graph for Voltage
        with(powercons,plot(Time,Voltage, 
                            type="l", xlab="datetime"))
        
        # draw the graph for Energy sub metering 
        with(powercons,plot(Time,Sub_metering_1, type="n", xlab="",
                            ylab="Energy sub metering"))
        with(powercons, points(Time,Sub_metering_1, type="l"))
        with(powercons, points(Time,Sub_metering_2, type="l", col="red"))
        with(powercons, points(Time,Sub_metering_3, type="l", col="blue"))
        legend("topright",lty=1,col=c("black","red","blue"), 
               legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
               box.lty = 0, inset = .01)
        
        # draw the graph for Global reactive power
        with(powercons,plot(Time,Global_reactive_power, 
                            type="l", xlab="datetime"))
        
        # close the bitmap device
        dev.off()
}
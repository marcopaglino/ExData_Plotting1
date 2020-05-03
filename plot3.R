# plot3.R
# the function draw a plot based on the 
# measurements of electric power consumption in one household 
# from 2007-02-01 to 2007-02-02.
# The graph displays the energy consumption in the specified range
# for each of the three Sub Metering Variable
# 
plot3 <- function() {
        # load the dplyr library to manipulate data
        library(dplyr)
        # read raw data in a data.frame, extracting only the data from
        # from 2007-02-01 to 2007-02-02
        # selecting only Date, Time, Sub_metering_1 Sub_metering_2 Sub_metering_3
        # and converting the last three vars to numeric
        powercons <- read.csv2("data/household_power_consumption.txt", stringsAsFactors = F,
                               na.strings = "?") %>% 
                subset(Date=="1/2/2007" | Date=="2/2/2007") %>% 
                select(Date,Time, Sub_metering_1, Sub_metering_2,Sub_metering_3 ) %>%
                mutate(Sub_metering_1 = as.numeric(Sub_metering_1)) %>%
                mutate(Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
                mutate(Sub_metering_3 = as.numeric(Sub_metering_3))
        
        # converting Date and Time
        powercons$Time <- strptime(paste(powercons$Date,powercons$Time),"%d/%m/%Y %H:%M:%S")
        powercons$Date <- as.Date(powercons$Date,"%d/%m/%Y")
        
        # set the locale to english
        Sys.setlocale("LC_TIME", locale = "English")
        
        # open the bitmap device
        png(file="plot3.png")
        # do not draw the plot now
        with(powercons,plot(Time,Sub_metering_1, type="n", xlab="",
                            ylab="Energy sub metering"))
        # draw three different sequences of points
        with(powercons, points(Time,Sub_metering_1, type="l"))
        with(powercons, points(Time,Sub_metering_2, type="l", col="red"))
        with(powercons, points(Time,Sub_metering_3, type="l", col="blue"))
        # add a legend
        legend("topright",lty=1,col=c("black","red","blue"), 
               legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        # close the device
        dev.off()
        
}
# plot2.R
# the function draw a plot based on the 
# measurements of electric power consumption in one household 
# from the dates 2007-02-01 and 2007-02-02.
# The graph displays the household global minute-averaged 
# active power (in kilowatt) consumption in two days 
# on a minute basis
plot2 <- function() {
        # load the dplyr library to manipulate data
        library(dplyr)
        # read raw data in a data.frame, extracting only the data from
        # from 2007-02-01 to 2007-02-02
        # selecting only Date, Time and Global_active_power variables
        # and converting Global_active_power to numeric
        powercons <- read.csv2("data/household_power_consumption.txt", stringsAsFactors = F,
                               na.strings = "?") %>% 
                subset(Date=="1/2/2007" | Date=="2/2/2007") %>% 
                select(Date, Time, Global_active_power) %>% 
                mutate(Global_active_power = as.numeric(Global_active_power))
        
        # converting Date and Time
        powercons$Time <- strptime(paste(powercons$Date,powercons$Time),"%d/%m/%Y %H:%M:%S")
        powercons$Date <- as.Date(powercons$Date,"%d/%m/%Y")
        
        # set the locale to english
        Sys.setlocale("LC_TIME", locale = "English")
        
        # open the bitmap device
        png(file="plot2.png")
        # draw the graph
        with(powercons,plot(Time,Global_active_power, 
                            type="l", xlab="",
                            ylab="Global Active Power (kilowatt)"))
        # close the device
        dev.off()
        
}
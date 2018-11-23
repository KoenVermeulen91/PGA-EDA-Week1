### Peer-graded Assignment: Course Project 1 ###
###    Exploratory Data Analysis: Week 1     ###
###                PLOT 4                    ###
################################################

# Setting working directory
setwd("~/Downloads/Data_Science_Specialization/Tests/Tests_R/PGA-EDA-Week1")

# Loading libraries
library(dplyr)
library(lubridate)

# Downloading data
if (!file.exists("Data")) {dir.create("Data")}
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
              "Data/Household_power_consumption.zip")

# Unzipping folder
unzip("Data/Household_power_consumption.zip", exdir="./Data")

# Reading data
HPC <- tbl_df(read.csv("Data/household_power_consumption.txt", sep = ";"))

# Preperating data for analysis: changing classes to date and numeric
HPC <- HPC %>%
        mutate(Date.Time = as.POSIXct(paste(Date, Time, sep = " "), format = "%d/%m/%Y %H:%M:%S")) %>%
        mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
        mutate(Weekday = weekdays(Date, abbreviate = T)) %>%
        mutate(Global_active_power = as.numeric(as.character(Global_active_power))) %>%
        mutate(Global_reactive_power = as.numeric(as.character(Global_reactive_power))) %>%
        mutate(Voltage = as.numeric(as.character(Voltage))) %>%
        mutate(Global_intensity = as.numeric(as.character(Global_intensity))) %>%
        mutate(Sub_metering_1 = as.numeric(as.character(Sub_metering_1))) %>%
        mutate(Sub_metering_2 = as.numeric(as.character(Sub_metering_2))) %>%
        mutate(Sub_metering_3 = as.numeric(as.character(Sub_metering_3)))

# Selecting columns and filtering for the two days
HPC <- HPC %>%
        select(1, 10:11, 3:9) %>%
        filter(Date == "2007-02-01" | Date == "2007-02-02")

# Generating fourth plot
png("plot4.png", width = 480, height = 480)
par(mfrow = c(2, 2))
plot(HPC$Date.Time, HPC$Global_active_power, 
     type = "n",
     ylab = "Global Active Power",
     xlab = "")
lines(HPC$Date.Time, HPC$Global_active_power)
plot(HPC$Date.Time, HPC$Voltage,
     type = "n",
     xlab = "datetime",
     ylab = "Voltage")
lines(HPC$Date.Time, HPC$Voltage)
plot(HPC$Date.Time,HPC$Sub_metering_1,
     type = "n",
     ylab = "Energy sub metering",
     xlab = "")
lines(HPC$Date.Time,HPC$Sub_metering_1)
lines(HPC$Date.Time,HPC$Sub_metering_2,
      col = "red")
lines(HPC$Date.Time,HPC$Sub_metering_3,
      col = "blue")
legend("topright", 
       lty = c(1, 1, 1),
       col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       bty = "n")
plot(HPC$Date.Time, HPC$Global_reactive_power,
     type = "n",
     xlab = "datetime", 
     ylab = "Global_reactive_power")
lines(HPC$Date.Time, HPC$Global_reactive_power)
dev.off()
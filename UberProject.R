#Uber Project in R
#John Ridgeway
#January 2021


#Load necessary packages
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(tidyverse)

#Set colors
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")


#Read in raw uber data from each month
apr_data <- read.csv("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/Uber-dataset/uber-raw-data-apr14.csv")
may_data <- read.csv("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/Uber-dataset/uber-raw-data-may14.csv")
jun_data <- read.csv("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/Uber-dataset/uber-raw-data-jun14.csv")
jul_data <- read.csv("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/Uber-dataset/uber-raw-data-jul14.csv")
aug_data <- read.csv("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/Uber-dataset/uber-raw-data-aug14.csv")
sep_data <- read.csv("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/Uber-dataset/uber-raw-data-sep14.csv")

#Concatenate raw uber data from each month into one dataframe
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

#Reformat Date.Time column
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

#Add column to data_2014 with only time
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

# Reformat date
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

#Change variables from vectors to factors
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

#Creates dataframe of number of passengers at each hour of the day                       
hour_data <- data_2014 %>%
  group_by(hour) %>%
    dplyr::summarize(Total = n()) 

#Creates bar graph of hour_data
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


#Sorts data to only include weekends
ride_data_weekend <- data_2014 %>%
  filter((month == "Apr" & (((as.numeric(day) %% 7) == 5) | (as.numeric(day) %% 7) == 6)) |
           (month == "May" & (((as.numeric(day) %% 7) == 3) | (as.numeric(day) %% 7) == 4)) |
           (month == "Jun" & (((as.numeric(day) %% 7) == 0) | (as.numeric(day) %% 7) == 1)) |
           (month == "Jul" & (((as.numeric(day) %% 7) == 5) | (as.numeric(day) %% 7) == 6)) |
           (month == "Aug" & (((as.numeric(day) %% 7) == 2) | (as.numeric(day) %% 7) == 3)) |
           (month == "Sep" & (((as.numeric(day) %% 7) == 6) | (as.numeric(day) %% 7) == 0))) 

#Creates dataframe of number of passengers at each hour of the day on the weekend                      
hour_data_weekend <- ride_data_weekend %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 

#Creates bar graph of hour_data_weekend
ggplot(hour_data_weekend, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour on the Weekends") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#Sorts data to only include weekdays
ride_data_weekday <- data_2014 %>%
  filter((month == "Apr" & !(((as.numeric(day) %% 7) == 5) | (as.numeric(day) %% 7) == 6)) |
           (month == "May" & !(((as.numeric(day) %% 7) == 3) | (as.numeric(day) %% 7) == 4)) |
           (month == "Jun" & !(((as.numeric(day) %% 7) == 0) | (as.numeric(day) %% 7) == 1)) |
           (month == "Jul" & !(((as.numeric(day) %% 7) == 5) | (as.numeric(day) %% 7) == 6)) |
           (month == "Aug" & !(((as.numeric(day) %% 7) == 2) | (as.numeric(day) %% 7) == 3)) |
           (month == "Sep" & !(((as.numeric(day) %% 7) == 6) | (as.numeric(day) %% 7) == 0))) 

#Creates dataframe of number of passengers at each hour of the day on the weekdays                    
hour_data_weekday <- ride_data_weekday %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 

#Creates bar graph of hour_data_weekend
ggplot(hour_data_weekday, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour on Weekdays") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

 
#Install and load package to read in data weather from Excel  
library(readxl)  


#Load weather data for each month into R
april_weather_data <- read_excel("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/WeatherDataNYC2014.xlsx", sheet = "4-14_NYC_Weather")
may_weather_data <- read_excel("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/WeatherDataNYC2014.xlsx", sheet = "5-14_NYC_Weather")
june_weather_data <- read_excel("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/WeatherDataNYC2014.xlsx", sheet = "6-14_NYC_Weather")
july_weather_data <- read_excel("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/WeatherDataNYC2014.xlsx", sheet = "7-14_NYC_Weather")
august_weather_data <- read_excel("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/WeatherDataNYC2014.xlsx", sheet = "8-14_NYC_Weather")
september_weather_data <- read_excel("/Users/johnridgeway/Desktop/DataSciencePortfolio/R/UberProject/WeatherDataNYC2014.xlsx", sheet = "9-14_NYC_Weather")

#Create new data frame that holds total rides for each day
total_rides_per_day <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n()) 

#Combine all weather data
weather_data <- rbind(april_weather_data, may_weather_data, june_weather_data, july_weather_data, august_weather_data, september_weather_data)

#Combine weather and ride data
weather_ride_data <- as.vector(cbind(total_rides_per_day, weather_data))


#Group data based on different average temperatures at each day within 5 degree blocks
weather_ride_totals_Avg_by5 <- weather_ride_data %>%
  group_by(gr=cut(AvgTemperature, breaks= seq(39.999, 85, by = 5)) ) %>%
  summarize(Mean = mean(Total))

ggplot(weather_ride_totals_Avg_by5 , aes(gr, Mean)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle(("Avg no. of Trips from April - September by Avg Temp")) +
  xlab("Average Daily Temperature") + ylab("Total Rides") 


#Group data by weekday data only
weather_ride_data_weekday <- weather_ride_data %>%
  filter((month == "Apr" & !(((as.numeric(day) %% 7) == 5) | (as.numeric(day) %% 7) == 6)) |
           (month == "May" & !(((as.numeric(day) %% 7) == 3) | (as.numeric(day) %% 7) == 4)) |
           (month == "Jun" & !(((as.numeric(day) %% 7) == 0) | (as.numeric(day) %% 7) == 1)) |
           (month == "Jul" & !(((as.numeric(day) %% 7) == 5) | (as.numeric(day) %% 7) == 6)) |
           (month == "Aug" & !(((as.numeric(day) %% 7) == 2) | (as.numeric(day) %% 7) == 3)) |
           (month == "Sep" & !(((as.numeric(day) %% 7) == 6) | (as.numeric(day) %% 7) == 0))) 

#Group data based on different average temperatures at each day during the weekdays
weather_ride_totals_weekday_Avg_by5 <- weather_ride_data_weekday %>%
  group_by(gr=cut(AvgTemperature, breaks= seq(39.999, 95, by = 5)) ) %>%
  summarize(Mean = mean(Total))

ggplot(weather_ride_totals_weekday_Avg_by5 , aes(gr, Mean)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle(("Avg no. of Trips from April - September by Avg Temp (Wkdays)")) +
  xlab("Average Daily Temperature") + ylab("Total Rides") 

#Group data by weekend data only
weather_ride_data_weekend <- weather_ride_data %>%
  filter((month == "Apr" & (((as.numeric(day) %% 7) == 5) | (as.numeric(day) %% 7) == 6)) |
           (month == "May" & (((as.numeric(day) %% 7) == 3) | (as.numeric(day) %% 7) == 4)) |
           (month == "Jun" & (((as.numeric(day) %% 7) == 0) | (as.numeric(day) %% 7) == 1)) |
           (month == "Jul" & (((as.numeric(day) %% 7) == 5) | (as.numeric(day) %% 7) == 6)) |
           (month == "Aug" & (((as.numeric(day) %% 7) == 2) | (as.numeric(day) %% 7) == 3)) |
           (month == "Sep" & (((as.numeric(day) %% 7) == 6) | (as.numeric(day) %% 7) == 0))) 

#Group data based on different average temperatures at each day during the weekdays
weather_ride_totals_weekend_Avg_by5 <- weather_ride_data_weekend %>%
  group_by(gr=cut(AvgTemperature, breaks= seq(39.999, 95, by = 5)) ) %>%
  summarize(Mean = mean(Total))

ggplot(weather_ride_totals_weekend_Avg_by5 , aes(gr, Mean)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle(("Avg no. of Trips from April - September by Avg Temp (Wkends Only)")) +
  xlab("Average Daily Temperature") + ylab("Total Rides") 


#Change Precipitation to numerical type
weather_ride_data$Precipitation <- as.numeric(weather_ride_data$Precipitation)

#Discard rows with NAN for Precipitation
weather_ride_data1 <- na.omit(weather_ride_data)

#Group data based on different amounts of precipitation each day within 1 inch blocks
weather_ride_totals_Precipitation_by1 <- weather_ride_data1 %>%
  group_by(gr=cut(Precipitation, breaks= seq(-0.001, 5, by = 1)) ) %>%
  summarize(Mean = mean(Total))

ggplot(weather_ride_totals_Precipitation_by1 , aes(gr, Mean)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle(("Average number of Trips from April to September by Precipitation")) +
  xlab("Average Daily Temperature") + ylab("Total Rides") 



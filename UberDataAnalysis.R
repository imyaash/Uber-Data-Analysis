library(ggplot2)  # For data visualization
library(ggthemes) # Add-on to ggplot2
library(lubridate)  # Dealing with date and time in the dataset
library(dplyr)  # For data manipulation
library(tidyr)  # For tidying the data
library(DT) # To interact with javascript library "Datatables"
library(scales) # For better data visualization


## Creating vector of color to be implemented
colours <- c("#cc2610", "#665b5b", "#05a3a3", "#cfc8c8", "#ecf540", "#065ec9", "#e86bb0")


## Reading multiple csv files in one data frame
ubraprdata <- read.csv("uber-raw-data-apr14.csv")
ubrmaydata <- read.csv("uber-raw-data-may14.csv")
ubrjundata <- read.csv("uber-raw-data-jun14.csv")
ubrjuldata <- read.csv("uber-raw-data-jul14.csv")
ubraugdata <- read.csv("uber-raw-data-aug14.csv")
ubrsepdata <- read.csv("uber-raw-data-sep14.csv")

uberdata2014 <- rbind(ubraprdata, ubrmaydata, ubrjundata, ubrjuldata, ubraugdata, ubrsepdata)


## Formatting Date.Time column & creating factors of time oblects
uberdata2014$Date.Time <- as.POSIXct(uberdata2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
uberdata2014$Time <- format(as.POSIXct(uberdata2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")
uberdata2014$Date.Time <- ymd_hms(uberdata2014$Date.Time)
uberdata2014$Day <- factor(day(uberdata2014$Date.Time))
uberdata2014$Month <- factor(month(uberdata2014$Date.Time, label = TRUE))
uberdata2014$Year <- factor(year(uberdata2014$Date.Time))
uberdata2014$DayofWeek <- factor(wday(uberdata2014$Date.Time, label = TRUE))
uberdata2014$Hours <- factor(hour(hms(uberdata2014$Time)))
uberdata2014$Minutes <- factor(minute(hms(uberdata2014$Time)))
uberdata2014$Seconds <- factor(second(hms(uberdata2014$Time)))


## Plotting the uber trips by the hours in a day for the whole period
hours_data <- uberdata2014 %>%
  group_by(Hours) %>%
  dplyr::summarize(Total = n())
datatable(hours_data)   

## Creating additional visualisation using ggplot
ggplot(hours_data, aes(Hours, Total)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)  # The visualization shows how thw number of passengers on hourly basis through the data
# It can be observed that the number of passenger is highest from 3:00 PM to 9:00 PM

## Plotting the uber trips by Hours and Month
month_hours <- uberdata2014 %>%
  group_by(Month, Hours) %>%
  dplyr::summarize(Total = n())

ggplot(month_hours, aes(Hours, Total, fill = Month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by the Hours and Month") +
  scale_y_continuous(labels = comma)

## Plotting data by trips during every day of the month
day_data <- uberdata2014 %>%
  group_by(Day) %>%
  dplyr::summarize(Total = n())
 datatable(day_data)

 ggplot(day_data, aes(Day, Total)) +
   geom_bar(stat = "identity", fill = "steelblue") +
   ggtitle("Trips Every Day of the Month") +
   theme(legend.position = "none") +
   scale_y_continuous(labels = comma)
 
 
## Plotting the uber trips by Day and Month
 day_month_group <- uberdata2014 %>%
   group_by(Month, Day) %>%
   dplyr::summarize(Total = n())

 ggplot(day_month_group, aes(Day, Total, fill = Month)) +
   geom_bar(stat = "identity") +
   ggtitle("Trips by Day and Month") +
   scale_y_continuous(labels = comma) +
   scale_fill_manual(values = colours)    # From the resulting visualization we can observe that the number of trips by the day is fairly
 # evenly spread. besides 30th but that can mostly be attributated to an outlier in April
 
 ## Number of Trips taking place during months in a year
 # Visualizing the number of trips that are taking place each month of the year.
 
Month_Group <- uberdata2014 %>%
  group_by(Month) %>%
  dplyr::summarize(Total = n())
datatable(Month_Group)

ggplot(Month_Group , aes(Month, Total, fill = Month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colours)   # We observe that the number of passengers per month is increasing, while the month of September has the maximum passenger

month_weekday <- uberdata2014 %>%
  group_by(Month, DayofWeek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(Month, Total, fill = DayofWeek)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colours)

## Finding out the number of Trips by bases
ggplot(uberdata2014, aes(Base)) + 
  geom_bar(fill = "purple") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")   # Out of the 5 bases in the our data 'B02617' has the maximum traffic

# Visualizing trips by Bases and Month
ggplot(uberdata2014, aes(Base, fill = Month)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colours)

# Visualizing trips by Bases and DaysofWeek
ggplot(uberdata2014, aes(Base, fill = DayofWeek)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Day of Week") +
  scale_fill_manual(values = colours)


## Ploting a heatmap by Hour and Day
dayandhour <- uberdata2014 %>%
  group_by(Day, Hours) %>%
  dplyr::summarise(Total = n())
datatable(dayandhour)

ggplot(dayandhour, aes(Day, Hours, fill = Total)) +
  geom_tile(color = "cyan") +
  ggtitle("Heatmap by Hour and Day")


## Plotting Heatmap by Month and Day
ggplot(day_month_group, aes(Day, Month, fill = Total)) +
  geom_tile(color = "cyan") +
  ggtitle("Heatmap by Month and Day")


## Plotting heatmap by month and day of week
ggplot(month_weekday, aes(DayofWeek, Month, fill = Total)) +
  geom_tile(color = "cyan") +
  ggtitle("Heatmap by Month and Day of Week")


## Plotting a heatmap by month and bases
monthbases <- uberdata2014 %>%
  group_by(Base, Month) %>%
  dplyr::summarise(Total = n())

dayofweekbases <- uberdata2014 %>%
  group_by(Base, DayofWeek) %>%
  dplyr::summarise(Total = n())

ggplot(monthbases, aes(Base, Month, fill = Total)) +
  geom_tile(color = "cyan") +
  ggtitle("Heatmap by Month and Bases")


## Plotting heatmap by Bases and Day of Week
ggplot(dayofweekbases, aes(Base, DayofWeek, fill = Total)) +
  geom_tile(color = "cyan") +
  ggtitle("Heatmap by Bases and Day of Week")


## Creating a map visualization of rides
minlat <- 40.5774
maxlat <- 40.9176
minlong <- -74.15
maxlong <- -73.7004

ggplot(uberdata2014, aes(x = Lon, y = Lat)) +
  geom_point(size = 1, color = "cyan") +
  scale_x_continuous(limits = c(minlong, maxlong)) +
  scale_y_continuous(limits = c(minlat, maxlat)) +
  theme_map() +
  ggtitle("NYC map Based on Uber Rides During 2014 (April to September)")

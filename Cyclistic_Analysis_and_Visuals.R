# Load packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(ggplot2)

# Read the cleaned file to start the analysis process

all_rides <- read_csv('all_rides.csv')

# find out the percentage of membership
View(all_rides %>% 
       group_by(membership) %>% 
       summarise(number_of_ride = n()) %>%
       mutate(percentage=number_of_ride*100/sum(number_of_ride)))

# Summary function will check for Min, Max, Mean and Median
summary(all_rides$ride_length)

# Compare members and casual rides behavior based on the Min, Max, Mean and Median ride length
aggregate(all_rides$ride_length ~ all_rides$membership, FUN = max)
aggregate(all_rides$ride_length ~ all_rides$membership, FUN = min)
aggregate(all_rides$ride_length ~ all_rides$membership, FUN = mean)
aggregate(all_rides$ride_length ~ all_rides$membership, FUN = median)

# Order day of week from Sunday to Saturday
all_rides$day_of_week <- ordered(all_rides$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Find out the avg of ride length based on day of week for each type of membership
aggregate(all_rides$ride_length ~ all_rides$membership + all_rides$day_of_week, FUN = mean)

# Find out the type of bike used based on the membership
View(all_rides %>%
       group_by(membership,rideable_type) %>%
       summarise(number_of_ride = n()))

# Find out the the number of rides based on the membership and month
View(all_rides %>% 
       group_by(month,membership) %>% 
       summarise(number_of_ride = n()))

# Find out number of rides based on day of week and membership

View(all_rides %>%
       group_by(membership,day_of_week) %>%
       summarise(number_of_ride = n()))

# Creating visuals

# Create pie chart for percentage of membership during 3 months
x <- c(34.75669,65.24331)
labels <- c("Casual", "Members") 
pie(x,labels, main="Percentage of membership during 3 months")


# Let's visualize the number of rides by rider type
all_rides %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(membership, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(membership, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = membership)) +
  geom_col(position = "dodge") + labs(title="the number of rides by rider type")

# Let's visualize membership vs bike type
all_rides %>% 
  group_by(membership, rideable_type) %>%
  summarise(number_of_rides = n()) %>%
  mutate(percentage=number_of_rides*100/sum(number_of_rides)) %>%
  ggplot(aes(x = membership, y = percentage, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title="membership vs bike type")


# Let's visualize months vs percentage of rides
all_rides %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n()) %>% 
  mutate(percentage=number_of_rides*100/sum(number_of_rides))  %>% 
  ggplot(aes(x = percentage, y = month)) +
  geom_col(position = "dodge",fill = "#00abff") +
  labs(title="months vs percent number of rides")


# Let's visualize average ride length vs membership
all_rides %>% 
  group_by(membership) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x = membership, y = average_ride_length , group = 1)) +
  geom_point(colour = "red",size=4) +
  labs(title="average ride length vs membership")




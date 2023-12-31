# Install required packages

install.packages("tidyverse") # transforming data
install.packages("lubridate") # date formation
install.packages("dplyr") # data manipulation
install.packages("janitor") # cleaning data
install.packages("ggplot2") # creating graphics

# Load the packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(ggplot2)

# Let us read the data from the three files

df202309 <- read_csv("202309.csv")
df202310 <- read_csv("202310.csv")
df202311 <- read_csv("202311.csv")

# Validate the columns name before proccessing them

colnames(df202309)
colnames(df202310)
colnames(df202311)

# Inspect more in the data var type

str(df202309)
str(df202310)
str(df202311)

# Now can combine the 3 files safely using rbind since we check the columns match across all files.

all_rides <- rbind(df202309,df202310,df202311)

# Want to eliminate the columns will not be used in the analysis

all_rides <- all_rides %>%
select(-c(start_lat, start_lng, end_lat, end_lng))

# Change the column name to be more meaningful
all_rides <- rename(all_rides, membership = member_casual)

# Check the result so far
colnames(all_rides)

# Add column for ride month
all_rides$month <- months(as.Date(all_rides$started_at))

# Change the numbers to char for day_of_week var

all_rides <- all_rides %>% 
   mutate(day_of_week = recode(day_of_week
                               ,"1" = "Sunday"
                               ,"2" = "Monday"
                               ,"3" = "Tuesday"
                               ,"4" = "Wednesday"
                               ,"5" = "Thursday"
                               ,"6" = "Friday"
                               ,"7" = "Saturday"))

# Create ride length column and change the type

all_rides$ride_length <- (as.double(difftime(all_rides$ended_at, all_rides$started_at)))/60

# Ride length less than 0 will be removed

sum(all_rides$ride_length < 0, na.rm=TRUE)
all_rides <- all_rides[!(all_rides$ride_length<0),]

# Inspect
colnames(all_rides)
dim(all_rides)
head(all_rides)
summary(all_rides)

# Cleaning for empty/NA records
all_rides <- remove_empty(all_rides)
all_rides <- drop_na(all_rides)
all_rides <- remove_missing(all_rides)

# let’s inspect the new table after cleaning

colnames(all_rides)
dim(all_rides)
head(all_rides)
summary(all_rides)

# Save the cleaned file to use it for analyse step

write.csv(all_rides,file = "all_rides.csv", row.names = FALSE)

# Well done me!

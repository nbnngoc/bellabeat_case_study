# loading packages

library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(showtext)

font_add_google('Montserrat', 'mont') # loading my favorite font
showtext_auto()

# importing datasets

daily_activity <- read_csv('dailyActivity_merged.csv')
glimpse(daily_activity)

hourly_steps <- read_csv('hourlySteps_merged.csv')
glimpse(hourly_steps)

daily_sleep <- read_csv('sleepDay_merged.csv')
glimpse(daily_sleep)

# checking for number of observations

n_distinct(daily_activity$Id)
[1] 33
n_distinct(hourly_steps$Id)
[1] 33
n_distinct(daily_sleep$Id)
[1] 24

# checking for duplicates and deleting if any

sum(duplicated(daily_activity))
[1] 0
sum(duplicated(hourly_steps))
[1] 0
sum(duplicated(daily_sleep))
[1] 3

daily_sleep <- daily_sleep %>%
distinct()

# formatting date-time variables

daily_activity <- daily_activity %>%
  rename(Date = ActivityDate) %>%
  mutate(Date = as_date(Date, format = '%m/%d/%Y'))

hourly_steps <- hourly_steps %>% 
  rename(DateTime = ActivityHour) %>% 
mutate(DateTime = as.POSIXct(DateTime, format = '%m/%d/%Y %I:%M:%S %p" , tz = Sys.timezone()))

daily_sleep <- daily_sleep %>%
  rename(Date = SleepDay) %>%
mutate(Date = as_date(Date, format = '%m/%d/%Y %I:%M:%S %p' , tz = Sys.timezone()))

# merging datatsets

daily_activity_sleep <- merge(daily_activity, daily_sleep, by = c ("Id", "Date"))
glimpse(daily_activity_sleep)

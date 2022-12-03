# chart 1: Asleep minutes per weekday

  # adding a new column for weekdays 
  
daily_activity_sleep <- daily_activity_sleep %>%
  mutate(weekday = weekdays(date, abbreviate = TRUE))

daily_activity_sleep$weekday <- ordered(daily_activity_sleep$weekday, levels = c('Mon, 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))

  # constructing a dataframe computing the average time asleep for the users per day of week

weekday_steps_sleep <- daily_activity_sleep %>%
  group_by(weekday) %>%
  summarize (daily_sleep = mean(TotalMinutesAsleep))

glimpse(weekday_steps_sleep)

  # visualizing (bar chart)

ggplot(weekday_steps_sleep, aes(weekday, daily_sleep)) +
  geom_col(fill = '#fbd464', color = '#000000') +
  geom_hline(yintercept = 480) +
  theme_minimal() +
  theme(plot.title = element_text(margin = margin(0,0,10,0), family = 'mont'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, family = 'mont'),
        axis.text.y = element_text(family = 'mont')) +
  labs(title = 'Asleep minutes per weekday', x = '', y = '', fill = '')
  
# chart 2: Hourly steps throughout the day

  # adding a new column for time factor

hourly_steps <- hourly_steps %>%
  separate(date_time, into = c('Date', 'Time'), sep = '') %>%
  mutate(Date = ymd(Date))

  # constructing a data frame computing the average number of steps taken per time slot during the day

daily_hourly_steps <- hourly_steps %>%
  group_by(Time) %>%
  summarize(avg_steps = mean(StepTotal))

glimpse(daily_hourly_steps)

  # visualizing (cirle histogram)

daily_hourly_steps %>%
  ggplot() +
  theme_minimal() +
  coord_polar(theta = 'x', start = -.13, direction = 1) +
  geom_col(mapping = aes(x = Time, y = avg_steps, fill = avg_steps), fill = '#f39c63') + 
  scale_x_continuous(breaks = seq(0, 24), labels = seq(0, 24)) +
  scale_y_continuous(expand = c(0.2, 0.2), breaks = c(0, 100, 200, 300, 400, 500)) + 
  theme(plot.title = element_text(size = 10, margin = margin(0,0,10,0), family = 'mont', hjust = 0.5),
        axis.text.x = element_text(size = 8, family = 'mont'),
        axis.text.y = element_text(size = 8, family = 'mont'),
        legend.position = 'none') +
  labs(title = 'Hourly steps throughout the day', x = '', y = '')
  
# chart 3: Proportion of users per activity level

  # computing the average steps taken by users per day

daily_average <- daily_activity_sleep %>%
  group_by(Id) %>%
  summarise(avg_daily_steps = mean(TotalSteps), avg_daily_calories = mean(Calories), avg_daily_sleep = mean(TotalMinutesAsleep))
  
  # adding a new column to label each user by type

daily_average <- daily_average %>%
  mutate(user_type = case_when(
    avg_daily_steps < 5000 ~ 'Sedentary',
    avg_daily_steps >= 5000 & avg_daily_steps < 7499 ~ 'Lightly active', 
    avg_daily_steps >= 7500 & avg_daily_steps < 9999 ~ 'Fairly active', 
    avg_daily_steps >= 10000  ~ 'Very active'))

  # creating a new data frame computing the percentage of each type

user_type_percent <- daily_average %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total/totals) %>%
  mutate(labels = scales::percent(total_percent))

user_type_percent$user_type <- factor(user_type_percent$user_type , levels = c('Very active', 'Fairly active', 'Lightly active', 'Sedentary'))

  # visualizing (pie chart)

user_type_percent %>%
  ggplot(aes(x = '', y = total_percent, fill = user_type)) +
  geom_bar(stat = 'identity', color = 'black', width = 1) +
  coord_polar('y', start = 0) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 8, family = 'mont'),
        plot.title = element_text(size = 10, family = 'mont', margin = margin(0,0,-8,0), hjust = 0.5),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.title = element_text(size = 8, family = 'mont'),
        legend.text = element_text(size = 8, family = 'mont')) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = c('#1d957c', '#fbd464', '#f39c63', '#eb544b')) +
  geom_text(aes(label = labels), size = 3, family = 'mont', position = position_stack(vjust = 0.5)) +
  labs(title = 'Proportion of users per activity level', fill = 'User type')

# chart 4: Proportion of users per frequency of use

  # creating a data frame computing the number of days users wear their smart devices and label them by using frequency

user_frequency <- daily_activity_sleep %>% 
  group_by(Id) %>% 
  summarise(number_of_days = n()) %>%
  mutate(user_type = case_when(
    number_of_days <= 10 ~ 'Low use',
    number_of_days > 10 & number_of_days <= 20 ~ 'Moderate use', 
    number_of_days > 20 & number_of_days <= 31 ~ 'High use')
  )

user_frequency_percent <- user_frequency %>% # percentage labels added to better visualize the insights
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

user_frequency_percent$user_type <- factor(user_frequency_percent$user_type , levels = c("Low use", "Moderate use", "High use"))

  # visualizing (pie chart)

user_frequency_percent %>%
  ggplot(aes(x = "",y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity", color = "black", width = 1)+
  coord_polar("y", start = 0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 8, family = "mont"),
        plot.title = element_text(hjust = 0.5, size = 10, margin = margin(0,0,-8,0), family = "mont"),
        legend.spacing.y = unit(0.3, "cm"),
        legend.title = element_text(size = 8, family = "mont"),
        legend.text = element_text(size = 8, family = "mont")) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values = c("#caf0f8","#00b4d8", "#0077b6")) +
  geom_text(aes(label = labels), size = 3, family = "mont", position = position_stack(vjust = 0.5), color = c('white', 'black', 'black')) +
  labs(title = "Proportion of users per frequency of using", fill = "User type")

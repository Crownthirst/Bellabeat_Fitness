library(tidyverse)
library(readr)
library(lubridate)
library(skimr)
library(psych)
library(janitor)

#load our datasets
daily_activity <- read.csv('dailyActivity_merged.csv')
daily_intensity <- read.csv('dailyIntensities_merged.csv')
hourly_steps <- read.csv('hourlySteps_merged.csv')
sleep_day <- read.csv('sleepDay_merged.csv')

#preview our datasets
head(daily_activity)
head(daily_intensity)
head(hourly_steps)
head(sleep_day)

#Check for number of unique Id
n_unique(daily_activity$Id)
n_unique(daily_intensity$Id)
n_unique(hourly_steps$Id)
n_unique(sleep_day$Id)

#Check for duplicate in the data
sum(duplicated(daily_activity))
sum(duplicated(daily_intensity))
sum(duplicated(hourly_steps))
sum(duplicated(sleep_day))

#remove duplicate in sleep_day
sleep_day <- sleep_day %>% 
  distinct() %>% 
  drop_na()

#confirm there are no duplicates
sum(duplicated(sleep_day))

#remove days of inactivity 
daily_activity <-  filter(daily_activity, TotalSteps != 0)
daily_intensity <- filter(daily_intensity, SedentaryMinutes != 1440)


#check column specifications
str(daily_activity)
str(daily_intensity)
str(hourly_steps)
str(sleep_day)

#format dates as date
daily_activity <- daily_activity %>% 
  mutate(ActivityDate = as_date(ActivityDate, format = '%m/%d/%y'))
daily_intensity <- daily_intensity %>% 
  mutate(ActivityDay = as_date(ActivityDay, format = '%m/%d/%y'))
hourly_steps <- hourly_steps %>% 
  mutate(ActivityHour = as.POSIXct(ActivityHour,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
sleep_day <- sleep_day %>% 
  mutate(SleepDay = as.POSIXct(SleepDay,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

#clean names and rename columns
daily_activity <- clean_names(daily_activity) 
daily_intensity <- clean_names(daily_intensity)
hourly_steps <- clean_names(hourly_steps)
sleep_day <- clean_names(sleep_day)

#calculate average daily steps
daily_average <- daily_activity %>% 
  group_by(id) %>% 
  summarise(average_steps = mean(total_steps), average_calories = mean(calories), average_sedentary = mean(sedentary_minutes))
head(daily_average)
#classify users
user_type <- daily_average %>%
  mutate(user_type = case_when(
    average_steps < 5000 ~ "sedentary",
    average_steps >= 5000 & average_steps < 7499 ~ "low active", 
    average_steps >= 7500 & average_steps < 9999 ~ "somewhat active", 
    average_steps >= 10000 ~ "Highly active"
  ))

#Get average sleep in minutes per user
user_sleep <- sleep_day %>% 
  group_by(id) %>% 
  summarise(average_minute_asleep = mean(total_minutes_asleep))

#join sleep data with daily average data
daily_average <- daily_average %>% 
  left_join(user_sleep)
#calculate percentage distribution of users
percentage_user_type <- user_type %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

#create a pie chart for user activity
percentage_user_type %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666")) +
  scale_fill_manual(values = c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="User type distribution")
#Get Sedantary week day
weekday_inactivity <- daily_activity %>% 
  mutate(weekday = weekdays(activity_date))

weekday_inactivity$weekday <- ordered(weekday_inactivity$weekday, levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

weekday_inactivity <- weekday_inactivity %>% 
  group_by(weekday) %>%
  summarize (average_sedentary = mean(sedentary_minutes))
head(weekday_inactivity)
#visualizing result on a chart
weekday_inactivity %>%
  ggplot() +
  geom_col(mapping = aes(x=weekday, y = average_sedentary, fill = average_sedentary)) + 
  labs(title = "Users Inactive days", x="Weekday", y="Sedentary minutes") + 
  scale_fill_gradient(low = "green", high = "red")+
  theme(axis.text.x = element_text(angle = 90))

#sleepday activity
sleepday_activity <- sleep_day %>% 
  mutate(weekday = weekdays(sleep_day))

sleepday_activity$weekday <- ordered(sleepday_activity$weekday, levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

sleepday_activity <- sleepday_activity %>% 
  group_by(weekday) %>%
  summarize (average_sleep = mean(total_minutes_asleep))
head(sleepday_activity)
#visualize sleep activity
sleepday_activity %>%
  ggplot() +
  geom_col(mapping = aes(x=weekday, y = average_sleep, fill = average_sleep)) + 
  labs(title = "Users sleep activity", x="Weekday", y="average minute in asleep") + 
  scale_fill_gradient(low = "red", high = "green")+
  theme(axis.text.x = element_text(angle = 90))
#generate a correlation matrix amongst relevant variables
daily_average %>% 
  select(average_steps, average_calories) %>% 
  cor()
#visualize the relationship between steps and calories burnt
ggplot(data = daily_average, aes(x = average_steps, y = average_calories))+
         geom_point()+
         xlab('Steps') + ylab('Calories')+
        ggtitle('Relationship between steps and calories burnt')+
        geom_smooth(method = lm)

#generate a corellation matrix betwwen steps taken and minutes taken to sleep
daily_average %>% 
  select(average_steps, average_minute_asleep) %>% 
  drop_na() %>% 
  cor()

#visualize the relationship between steps and minutes asleep
ggplot(data = daily_average, aes(x = average_steps, y = average_minute_asleep))+
  geom_point()+
  xlab('Steps') + ylab('Sleep')+
  ggtitle('Relationship between steps and minutes asleep')+
  geom_smooth(method = lm)

#try to understand which time of the day our users are mostly active
hourly_steps <- hourly_steps %>%
  separate(activity_hour, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date)) 

head(hourly_steps)

#visualize the activity hour
hourly_steps %>%
  group_by(time) %>%
  summarize(average_steps = mean(step_total)) %>%
  ggplot() +
  geom_col(mapping = aes(x=time, y = average_steps, fill = average_steps)) + 
  labs(title = "User most active hours", x="", y="") + 
  scale_fill_gradient(low = "red", high = "green")+
  theme(axis.text.x = element_text(angle = 90))
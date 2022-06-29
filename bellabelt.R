#Require libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(hms)
library(readr)

#Import files 
activity <- read.csv("C:/Users/Amr 10/Downloads/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
calories <- read.csv("C:/Users/Amr 10/Downloads/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
intensities <- read.csv("C:/Users/Amr 10/Downloads/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
sleep <- read.csv("C:/Users/Amr 10/Downloads/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight <- read.csv("C:/Users/Amr 10/Downloads/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
heart <- read.csv("C:/Users/Amr 10/Downloads/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

#Fix date format

Sys.setlocale("LC_TIME","C")

 # Activity date
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
activity$weekday <- format(activity$ActivityDate, format = "%A")
activity$weekday <- factor(activity$weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # Calories date
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

  # Intensities date
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")

  # sleep 
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")

  # weight 
weight$Date_Time=as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$Date <- format(weight$Date_Time, format = "%m/%d/%y")
weight$time <- format(weight$Date_Time, format = "%H:%M:%S")
  # Heart 
heart$Date_Time=as.POSIXct(heart$Time, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
heart$Date <- format(heart$Date_Time, format = "%m/%d/%y")
heart$time <- format(heart$Date_Time, format = "%H:%M:%S")

#Remove duplicates 
 
  #Activity 
activity <- activity %>%
  distinct() %>%
  clean_names() %>%
  drop_na()

  #Calories
calories <- calories %>%
  distinct() %>%
  clean_names() %>%
  drop_na()

  #Intensities
Intensities <- Intensities %>%
  distinct() %>%
  clean_names() %>%
  drop_na()%>% 
  intensities <- rename(intensities,id = Id)

  #sleep 
sleep <- sleep %>%
  distinct() %>%
  clean_names() %>%
  drop_na()

  #weight 
weight <- weight %>% 
  distinct() %>%
  clean_names() %>%
  drop_na()

  #heart 
heart <- heart %>% 
  distinct() %>%
  clean_names() %>%
  drop_na()

#Merge activity with weight and Visualize 
activity_with_weight <- merge(activity,weight, by= 'id')

activity_with_weight %>% 
  select(total_steps,weekday,weight_kg,fat,bmi,calories) %>%
  summary()

  ggplot(data = activity_with_weight,aes(total_steps,calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")

  ggplot(data = activity_with_weight, aes(weekday,total_steps)) +
    geom_col() +
    geom_text(aes(label = total_steps), vjust = 0, colour = "white",check_overlap = TRUE) +
    labs(title="total_steps vs. weekday")
  

#merge activity with sleep and visualize
  activity_with_sleep <- merge(activity,sleep, by= "id")

    ggplot(data =activity_with_sleep,aes(total_minutes_asleep, sedentary_minutes)) +
    geom_point() + geom_smooth()+  labs(title="Minutes Asleep vs. Sedentary Minutes")

  ggplot(data =activity_with_sleep,aes(total_minutes_asleep, calories))+
    geom_point() + geom_smooth()+  labs(title="Minutes Asleep vs. losingcalories")
  
#merge activity with intensities 
  activity_with_intensities <- merge(activity,intensities, by=c("id", "date"))

   activity_with_intensities %>% 
    select(total_steps, time,weekday,TotalIntensity) %>%
    group_by(time) %>% 
    summarize(mean_total_int = mean(TotalIntensity)) %>% 
     ggplot(aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
     theme(axis.text.x = element_text(angle = 90)) +
     labs(title="Average Total Intensity vs. Time")
   
library(tidyverse)
library(tidyr)
library(dplyr)
library(assertive)
library(ggplot2)
library(stringr)
library(skimr)
library(lubridate)



data <- read.csv("C:/Users/Memen/Desktop/BikeShare-use-case/Datasets-of-months-2019/Divvy_Trips_2019_Q1/Divvy_Trips_2019_Q1.csv")
data<-data[ , !(names(data) %in% c('X','X.1'))]

skim_without_charts(data)


l<-data$end_time 


data<-data %>%
        mutate(start_time =mdy_hm(start_time),end_time=mdy_hm(end_time))

sum(duplicated(data))
#make sure the data in the past and there is no out range of the dates 
assert_all_are_in_past(data$start_time)
assert_all_are_in_past(data$end_time)
l<-c('5-1-2021','10-10-2021')
assert_all_are_in_past(mdy(l))

s<-data %>%
  group_by(start_time) %>%
  summarise(Total= n())


summary(data$birthyear)

data<-data %>%
       mutate(duration_trip_in_min = difftime(end_time,start_time,units = "mins"))

data <- data%>%
            mutate(duration_trip_in_min = as.numeric(duration_trip_in_min))

mean(data$duration_trip_in_min)
quantile(data$duration_trip_in_min)

data %>% 
  select(names(data))%>%
  filter(duration_trip_in_min <1440)


gender_duration <- data %>% 
                      group_by(gender)%>%
                      summarise(Total_Minute = sum(duration_trip_in_min))
aged_duration <- data %>% 
  group_by(birthyear)%>%
  summarise(Total_Minute = sum(duration_trip_in_min))


ggplot(data = data, mapping=aes(x=gender,fill=gender))+
  geom_bar()

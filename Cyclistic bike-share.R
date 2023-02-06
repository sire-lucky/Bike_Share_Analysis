install.packages("geosphere")
#Loading packages
library(tidyverse)
library(readr)
library(skimr)
library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)
library(geosphere)

#importing data set
data1 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202109-divvy-tripdata.csv")
data2 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202110-divvy-tripdata.csv")
data3 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202111-divvy-tripdata.csv")
data4 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202112-divvy-tripdata.csv")
data5 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202201-divvy-tripdata.csv")
data6 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202202-divvy-tripdata.csv")
data7 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202203-divvy-tripdata.csv")
data8 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202204-divvy-tripdata.csv")
data9 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202205-divvy-tripdata.csv")
data10 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202206-divvy-tripdata.csv")
data11 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202207-divvy-tripdata.csv")
data12 <- read.csv("C:/Users/DELL/Documents/Cyclistic bike-share/202208-divvy-tripdata.csv")

#merging the data-set 
Bike_rides <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10,
                        data11, data12)

#taking a look/preview at the data sets
head(Bike_rides)
glimpse(Bike_rides)
str(Bike_rides)
skim_without_charts(Bike_rides) #quick summary of the data set

#renaming columns 
names(Bike_rides) [2] <- 'bike'
names(Bike_rides) [13] <- 'user'

#checking and remover of NA 
colSums(is.na(Bike_rides))
sum(is.na(Bike_rides))
Bike_rides <- drop_na(Bike_rides)

#checking for duplicate
sum(duplicated(Dactivity))

#checking for blank space 
is.null(Bike_rides)

#ensuring consistency in date and time columns
Bike_rides[['ride_length']] <- as.POSIXct(Bike_rides[['ride_length']], format = "%H:%M:%S")

Bike_rides$started_at <- mdy_hm(Bike_rides$started_at)

Bike_rides$ended_at <- mdy_hm(Bike_rides$ended_at)

str(Bike_rides[['ride_length']])
# splitting date-time columns into different columns 
Bike_rides <- separate(Bike_rides, "started_at", into = c('start_date', 'start_time'), sep = ' ')

Bike_rides <- separate(Bike_rides, "ended_at", into = c('end_date', 'end_time'), sep = ' ')

Bike_rides <- separate(Bike_rides, "ride_length", into = c('ride_length_date', 'ride_length_time'), sep = ' ')

#ensuring consistency in date and time columns
Bike_rides$start_date <- ymd(Bike_rides$start_date)
Bike_rides$end_date <- ymd(Bike_rides$end_date)
Bike_rides$ride_length_date <- ymd(Bike_rides$ride_length_date)

Bike_rides$start_time <- lubridate::hms(Bike_rides$start_time)
Bike_rides$end_time <- lubridate::hms(Bike_rides$end_time)
Bike_rides$ride_length_time <- lubridate::hms(Bike_rides$ride_length_time)
str(Bike_rides$start_time)
str(Bike_rides$end_time)
str(Bike_rides$ride_length_time)

# to remove (ride_length_time)rows with negative values and less than one minutes
Bike_rides <- Bike_rides[!Bike_rides$ride_length_time < 1,]
#or

Bike_rides <- Bike_rides %>% 
  filter(ride_length_time > 0)

# remove rows where rides were above one day
Bike_rides <- Bike_rides[!Bike_rides$ride_length_time > 1440,]

# to change (day_of_week)column from numbers to letter
Bike_rides$day_of_week <- weekdays(Bike_rides$start_date)

######### Analyze
# ride summary
Bike_rides %>% 
  group_by(user) %>% 
  summarise(average_ride_time = mean(ride_length_time),
            max_ride_time = max(ride_length_time),
            min_ride_time = min(ride_length_time))

# user count 
user_count <- Bike_rides %>% 
  group_by(user) %>% 
  summarise(total = n()) %>% 
  mutate(overall_total = sum(total)) %>% 
  group_by(user) %>% 
  summarise(percent_total = total/overall_total)

glimpse(user_count)

### bike usage over the pass 12months in percentage 
trips <- Bike_rides %>% 
  group_by(bike) %>% 
  summarise(total = n()) %>% 
  mutate(overall_total = sum(total)) %>% 
  group_by(bike) %>% 
  summarise(percent_total = total/overall_total)

glimpse(trips)

## member bike type used
member_biketype_used <- Bike_rides %>%
  filter(user == "member") %>% 
  group_by(bike) %>% 
  summarise(total = n()) %>% 
  mutate(overall_total = sum(total)) %>% 
  group_by(bike) %>% 
  summarise(percent_member = total/overall_total)

member_biketype_used$percent_member <- round(member_biketype_used$percent_member, digits = 3)

glimpse(member_biketype_used)

##casual bike type used
casual_member_biketype <- Bike_rides %>% 
  filter(user == "casual") %>% 
  group_by(bike) %>% 
  summarise(total = n()) %>% 
  mutate(overall_total = sum(total)) %>% 
  group_by(bike) %>% 
  summarise(percent_casual = total/overall_total)

casual_member_biketype$percent_casual <- round(casual_member_biketype$percent_casual, digits = 3)

glimpse(casual_member_biketype)

## average/mean time of riders time per user 
rider_time_user <- Bike_rides %>% 
  group_by(user) %>% 
  summarise(average_ride = mean(ride_length_time), .groups = "drop")

rider_time_user$average_ride <- round(rider_time_user$average_ride, digits = 2)

glimpse(rider_time_user)

#average time spent on different bikes 
rider_time_biketypes <- Bike_rides %>% 
  group_by(bike, user) %>% 
  summarise(average_ride = mean(ride_length_time), .groups = "drop")

rider_time_biketypes$average_ride <- round(rider_time_biketypes$average_ride, digits = 3)

glimpse(rider_time_biketypes)

#ride count by user per weekday
ridecount_week <- Bike_rides %>% 
  group_by(user, day_of_week) %>% 
  summarise(numberofriders = n(), .groups = "drop")

glimpse(ridecount_week)

# ride count by user & month
ridecount_month <- Bike_rides %>% 
  group_by(user, month = case_when(
    month(lubridate::as_date(start_date)) == 01 ~ 'jan',
    month(lubridate::as_date(start_date)) == 02 ~ 'feb',
    month(lubridate::as_date(start_date)) == 03 ~ 'march',
    month(lubridate::as_date(start_date)) == 04 ~ 'april',
    month(lubridate::as_date(start_date)) == 05 ~ 'may',
    month(lubridate::as_date(start_date)) == 06 ~ 'june',
    month(lubridate::as_date(start_date)) == 07 ~ 'july',
    month(lubridate::as_date(start_date)) == 08 ~ 'aug',
    month(lubridate::as_date(start_date)) == 09 ~ 'sept',
    month(lubridate::as_date(start_date)) == 10 ~ 'oct',
    month(lubridate::as_date(start_date)) == 11 ~ 'nov', TRUE ~ 'dec')) %>% 
  summarise(numberofriders = n(), .groups = "drop") %>% 
  arrange(desc(numberofriders))

glimpse(ridecount_month)

#the number of rides per season
ridecount_season <- Bike_rides %>% 
  group_by(user, season = case_when(
    month(lubridate::as_date(start_date)) == 12 ~ 'winter',
    month(lubridate::as_date(start_date)) == 01 ~ 'winter',
    month(lubridate::as_date(start_date)) == 02 ~ 'winter',
    month(lubridate::as_date(start_date)) == 03 ~ 'spring',
    month(lubridate::as_date(start_date)) == 04 ~ 'spring',
    month(lubridate::as_date(start_date)) == 05 ~ 'spring',
    month(lubridate::as_date(start_date)) == 06 ~ 'summer',
    month(lubridate::as_date(start_date)) == 07 ~ 'summer',
    month(lubridate::as_date(start_date)) == 08 ~ 'summer', TRUE ~ 'autumn')) %>% 
  summarise(numberofriders = n(), .groups = "drop") %>% 
  arrange(season)

glimpse(ridecount_season)

# the most preferred bike type in percentage 
most_preferred_bike <- Bike_rides %>% 
  group_by(bike) %>% 
  summarise(numberofusage = n()) %>% 
  arrange(desc(numberofusage)) %>% 
  mutate(percentage = round(numberofusage/sum(numberofusage),4)*100)

glimpse(most_preferred_bike)

#the most preferred time of the day
most_preferred_time <- Bike_rides %>% 
  group_by(user, time_of_day = case_when(
    start_time >= "06:00" & start_time <= "11:59" ~ "morning",
    start_time >= "12:00" & start_time <= "16:00" ~ "afternoon",
    start_time >= "16:01" & start_time <= "19:00" ~ "evening", TRUE ~ "night")) %>% 
  summarise(numberofriders = n(), .groups = "drop") %>% 
  arrange(desc(numberofriders))

glimpse(most_preferred_time)

##SHARE
#DATA VISUALIZATION

# distribution of riders
ggplot(user_count, aes(x = '', y = percent_total, fill = user)) +
  geom_col(color = "grey20", size = 0.7) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = scales :: percent(percent_total)), position = position_stack(vjust = 0.5), size = 10, fontface = "bold", color = "#46465e") +
  scale_fill_manual(values = c("#91d8e0", "#42a0ab"), name = "type of riders", breaks = c("casual", "member"), labels = c("casual member", "annual member")) +
  labs(title = "Distribution of Riders", subtitle = "what % of riders are using the cyclistic", caption = "Data: Motivate International") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold", color = "#42a0ab"),
        plot.subtitle = element_text(hjust = 0.5, size = 16, face = "bold", color = "grey20"),
        plot.caption = element_text(size = 8, color = "grey35"),
        legend.title = element_text(size = 16, face = "bold", color = "#42a0ab"),
        legend.text = element_text(size = 14, color = "grey20"))

## day of the week ride 
Bike_rides %>% 
  group_by(user, day_of_week) %>% 
  summarise(number_of_riders = n(), .groups = "drop") %>%
  ggplot(aes(reorder(day_of_week, number_of_riders), number_of_riders, fill = user)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(title = "ride by day of week") +
  theme(panel.spacing = unit(4, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("casual" = "#be9e6f", "member" = "#2a603b")) +
  theme(legend.justification = c("right", "top")) +
  theme(legend.background = element_rect(fill = "#d9dfe0",
                                         size = 0.5, linetype = "solid")) +
  theme(plot.background = element_rect(fill = "#d9dfe0")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold"))

# the most preferred bike type
ggplot(most_preferred_bike, aes(x = 2, y = percentage, fill = bike)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 200) +
  geom_text(aes(label = paste(percentage, "%", sep = "")), col = "black") +
  ggtitle("most preferred bike type") +
  theme_void() +
  theme(legend.justification = c("right", "top")) +
  scale_fill_manual(values = c("#BE9E6F", "#568203", "#05472a")) +
  xlim(0.5, 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.background = element_rect(fill = "#D9DFE0")) +
  theme(legend.key = element_rect(fill = "black", colour = "black"))

# the most preferred time of the day 
ggplot(most_preferred_time, aes(x = time_of_day, y = numberofriders, fill = user, group = user)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#91d8e0", "#42a0ab"), name = "types of riders", breaks = c("casual", "member"),
                    labels = c("casual member", "annual member")) +
  labs(title = "most_preferred_time", subtitle = "rides happends the most at night",
       caption = "data: motivate international",
       x = "day of week", y = "number of rides") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
        plot.caption = element_text(size = 8, color = "grey35"),
        axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.text.x = element_text(size = 10, colour = "grey20", face = "bold"),
        axis.text.y = element_blank(), legend.position = "none")

## Import Dataset
library(readr)
combined_data <- read_csv("C:/Users/salim/Dropbox/Ismail/GWG-Data-Analysis-Capstone-Projects/Cyclistic Trip Data/combined_data.csv")
cyclistic <- combined_data
View(cyclistic)

## Adding Columns
cyclistic$ride_length <- difftime(cyclistic$ended_at, cyclistic$started_at, units = c("mins"))
cyclistic$ride_day <- format(as.Date(cyclistic$started_at), "%A")
cyclistic$ride_month_year <- format(as.Date(cyclistic$started_at), "%m")

library(dplyr)
library(geosphere)
cyclistic <- cyclistic %>%
  rowwise() %>%
  mutate(ride_distance = distHaversine(c(start_lng, start_lat), c(end_lng, end_lat)))

## Cleaning Data

cyclistic_v2 <- cyclistic %>% filter(ride_length >=0, ride_distance != 0)
cyclistic_v2$ride_day<- ordered(cyclistic_v2$ride_day, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

## Summary Statistics - not added to report
mean_cyc = aggregate(cyclistic_v2$ride_length ~ cyclistic_v2$member_casual, FUN = median)
med_cyc = aggregate(cyclistic_v2$ride_length ~ cyclistic_v2$member_casual, FUN = median)
max_cyc = aggregate(cyclistic_v2$ride_length ~ cyclistic_v2$member_casual, FUN = max)
min_cyc = aggregate(cyclistic_v2$ride_length ~ cyclistic_v2$member_casual, FUN = min)

Ride_Length_Stat <- c("Mean", "Median", "Max", "Min")
Casual <- c(mean_cyc[1,2], med_cyc[1,2], max_cyc[1,2], min_cyc[1,2])
Annual <- c(mean_cyc[2,2], med_cyc[2,2], max_cyc[2,2], min_cyc[2,2])

summary_df <- data.frame(Ride_Length_Stat, Casual, Annual)

# Boxplots Comparing Ride Length
ggplot(cyclistic_v2, aes(x=member_casual, y=ride_length, fill=member_casual)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-10, 100)) +
  labs(title="Comparing Ride Lengths Between Member Types", x="Member Type", y="Ride Length (mins)", fill="Member Type") 

# Boxplots Comparing Ride Distance
ggplot(cyclistic_v2, aes(x=member_casual, y=ride_distance, fill=member_casual)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-10, 10000)) +
  labs(title="Comparing Ride Distances Between Member Types", x="Member Type", y="Ride Length (mins)", fill="Member Type") 

#Bike Type Comparison
cyclistic_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  ggplot(aes(x = member_casual, y = number_of_rides, fill = rideable_type)) + 
  geom_col(position = "stack") +
  labs(title = "Bike Types Used Compared Between Casual and Annual Members", x = "Member Type", y = "Number of Rides", fill = "Bike Type")

#Day Comparison
cyclistic_v2 %>% 
  group_by(member_casual, ride_day) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  ggplot(aes(x = ride_day, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") +
  labs(title = "Riding Days Compared Between Casual and Annual Members", x = "Day of The Week", y = "Number of Rides", fill = "Member Type")

#Distance
cyclistic_v2 %>% 
  group_by(member_casual) %>% 
  ggplot(aes(x=ride_id, y=ride_distance, shape = member_casual, color = member_casual)) + 
  geom_point(alpha = 0.3)+
  labs(title = "Riding Distance Compared Between Casual and Annual Members", color = "Member Type")


cyclistic_v2 %>% group_by(member_casual, ride_day, rideable_type) %>% summarise(number_of_rides = n(), avg_duration = mean(ride_length), avg_distance = mean(ride_distance)) %>% arrange(member_casual, ride_day)


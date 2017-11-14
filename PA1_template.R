library(dplyr)
library(ggplot2)

url <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "ActivityData.zip")
file <- unzip("ActivityData.zip")
Activity_Data <- read.csv("activity.csv")
Activity_Data$date <- as.Date(Activity_Data$date)


Total_Steps <- Activity_Data %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps))
 
ggplot(Total_Steps, aes(x = total_steps)) +
  geom_histogram(fill = "blue") +
  labs(x = "Steps", title = "Total Steps Per Day")

steps_mean <- mean(Total_Steps$total_steps, na.rm = TRUE)
steps_mean
steps_median <- median(Total_Steps$total_steps, na.rm = TRUE)
steps_median

Average_Daily <- Activity_Data %>%
  group_by(interval) %>%
  summarize(steps_avg = mean(steps, na.rm = TRUE))

ggplot(Average_Daily, aes(x = interval, y = steps_avg)) +
  geom_line() +
  labs(x = "Interval", y = "Average Steps", 
       title = "Average Steps Per Interval")

max <- Average_Daily[which.max(Average_Daily$steps_avg), ]
max

total_na <- sum(is.na(Activity_Data$steps))
total_na



#Imputed Mean
Activity_Data_2 <- Activity_Data
ind <- which(is.na(Activity_Data_2$steps))
Activity_Data_2[ind, "steps"] <- median(Average_Daily$steps_avg)

Total_Steps_2 <- Activity_Data_2 %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps))


ggplot(Total_Steps_2, aes(x = total_steps)) +
  geom_histogram(fill = "red") +
  labs(x = "Steps", title = "Total Steps Per Day")

steps_mean <- mean(Total_Steps_2$total_steps, na.rm = TRUE)
steps_mean
steps_median <- median(Total_Steps_2$total_steps, na.rm = TRUE)
steps_median

Average_Daily_2 <- Activity_Data_2 %>%
  group_by(interval) %>%
  summarize(steps_avg = mean(steps, na.rm = TRUE))

ggplot(Average_Daily_2, aes(x = interval, y = steps_avg)) +
  geom_line() +
  labs(x = "Interval", y = "Average Steps", 
       title = "Average Steps Per Interval")

max <- Average_Daily_2[which.max(Average_Daily$steps_avg), ]
max

Activity_Data_2_Days <- Activity_Data_2 %>%
  mutate(Weekday = weekdays(Activity_Data_2$date)) 

Weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
Weekends <- c("Saturday", "Sunday")

Weekday_Activity <- Activity_Data_2_Days %>%
  filter(Weekday %in% Weekdays)
Average_Daily_weekday <- Weekday_Activity %>%
  group_by(interval) %>%
  summarize(steps_avg = mean(steps, na.rm = TRUE))


Weekend_Activity <- Activity_Data_2_Days %>%
  filter(Weekday %in% Weekends)
Average_Daily_weekend <- Weekend_Activity %>%
  group_by(interval) %>%
  summarize(steps_avg = mean(steps, na.rm = TRUE))

par(mfrow = c(1, 2))
plot(Average_Daily_weekday$interval, Average_Daily_weekday$steps_avg,
     type = "l", xlab = "Interval", ylab = "Average Steps", 
     main = "Weekday Average Steps")
plot(Average_Daily_weekend$interval, Average_Daily_weekend$steps_avg,
     type = "l", xlab = "Inverval", ylab = "Average Steps",
     main = "Weekend Average Steps")


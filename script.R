df <- read.csv(unz("activity.zip","activity.csv"), header = TRUE, sep = ",")
df[,"date_formatted"] <- with(df, as.POSIXct(date, format = "%Y-%m-%d"))
df[,"time_formatted"] <- with(df, as.POSIXct(paste(floor(interval/100), interval %% 100, sep = ":"), format = "%H:%M"))

daily_steps <- aggregate(steps~date_formatted,data = df, sum)
hist(daily_steps$steps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps per day", ylab = "Frequency")

mn <- mean(daily_steps$steps)
mdn <- median(daily_steps$steps)

time_series <- aggregate(steps~time_formatted+interval,data = df, mean)
plot(time_series$time_formatted,time_series$steps,type="l", main = "Time series plot of the average number of steps taken", xlab = "Time of the day", ylab = "Average number of steps taken")

time_max <- format(time_series[which.max(time_series$steps),1], "%H:%M")

sum(is.na(df$steps))

df_imputed <- df
for(i in 1:nrow(time_series)){
  df_imputed[which(is.na(df$steps) & df$time_formatted == time_series[i,1]), "steps"] <- time_series[i,"steps"]
}

daily_steps_imputed <- aggregate(steps~date_formatted,data = df_imputed, sum)
hist(daily_steps_imputed$steps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps per day", ylab = "Frequency")

mn_imputed <- mean(daily_steps_imputed$steps)
mdn_imputed <- median(daily_steps_imputed$steps)

total_steps <- sum(df$steps)
total_steps_imputed <- sum(df_imputed$steps)

df_imputed[,"day_type"] <- "weekday"
df_imputed[which(weekdays(df_imputed$date_formatted) %in% c("Saturday","Sunday")),"day_type"] <- "weekend"
df_imputed$day_type <- as.factor(df_imputed$day_type)

time_series_imputed <- aggregate(steps~interval+day_type,data = df_imputed, mean)

library(lattice)
xyplot(steps~interval|day_type,data = time_series_imputed,layout = c(1,2), type = "l", main = "Time series plot with imputed data", xlab = "Interval", ylab = "Number of Steps")
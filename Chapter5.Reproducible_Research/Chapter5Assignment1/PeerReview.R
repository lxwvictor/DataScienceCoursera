activity <- read.csv("./activity.csv", stringsAsFactors = F)
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
activity$day <- weekdays(activity$date)
activity$daytype = as.factor(ifelse(activity$day %in% c('Saturday', 'Sunday'), "weekend", "weekday"))
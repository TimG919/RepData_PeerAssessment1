# Step 1

library(ggplot2)

      # unzip file if not already done and read csv
if (exists("activity.csv")) {
      activity <- read.csv("activity.csv")
}     else {
      unzip("./activity.zip")
      activity <- read.csv("activity.csv")
}
      
# totalStepsbyDay <- aggregate(steps ~ date, activity, sum)

# summarize and clean data
totalStepsbyDay <- with(activity,aggregate(steps, by = list(date),sum))
totalStepsbyDay <- na.omit(totalStepsbyDay)
colnames(totalStepsbyDay) <- c('Date','Steps')
totalStepsbyDay$Date <- as.Date(totalStepsbyDay$Date,"%Y-%m-%d")
# totalStepsbyDay$x <- as.numeric(totalStepsbyDay$x)


# Step 2
hist(totalStepsbyDay$Steps, xlab = "Total Steps", 
     main = "Histogram Total Steps Taken each Day")
#barplot(Steps~Date,data = totalStepsbyDay, ylab = "Total Steps")

# Step 3
mean(totalStepsbyDay$Steps)
median(totalStepsbyDay$Steps)

# Step 4
avgDailyPattern <- with(activity,aggregate(steps, by = list(interval),mean, na.rm = TRUE))
colnames(avgDailyPattern) <- c('Interval','AvgSteps')
plot(avgDailyPattern$Interval,avgDailyPattern$AvgSteps,type = "l",
     xlab = "Interval", ylab = "Average Steps", main = "Time Series of Average Steps Taken")
# qplot(Interval,AvgSteps,data = avgDailyPattern, geom = "line")

# Step 5
avgDailyPattern[which.max(avgDailyPattern$AvgSteps),]

# Step 6
# activity dataset contains 2304 missing values in the steps field
# will use mean for that 5 minute interval

activity2 <- activity
activity2$newSteps <- ave(activity2$steps, activity2$interval, FUN = function(x)mean(x[!is.na(x)]))
activity2$steps <- ifelse(is.na(activity2$steps),activity2$newSteps,activity2$steps)
totalStepsbyDayClean <- with(activity2,aggregate(steps, by = list(date),sum))
colnames(totalStepsbyDayClean) <- c('Date','Steps')
totalStepsbyDayClean$Date <- as.Date(totalStepsbyDayClean$Date,"%Y-%m-%d")

#activity2$steps[activity2$steps == "NA"] <-  replace(activty2$steps,activty2$newSteps)
#activty2 <- replace(activity2$steps,activty2$steps == "NA",activity2$newSteps)

# Step 7
hist(totalStepsbyDayClean$Steps, xlab = "Total Steps", 
     main = "Histogram Total Steps Taken each Day")
mean(totalStepsbyDayClean$Steps)
median(totalStepsbyDayClean$Steps)

# Step 8
activity3 <- activity2
activity3$date <- as.Date(activity3$date,"%Y-%m-%d")
activity3$type <- factor(grepl("S.+",weekdays(activity3$date)),levels = c(FALSE,TRUE),
                         labels = c("weekday","weekend"))


avgDailyPatternClean <- with(activity3,aggregate(steps, by = list(interval,type),mean))
colnames(avgDailyPatternClean) <- c('Interval','Type','AvgSteps')
qplot(Interval,AvgSteps,data = avgDailyPatternClean, facets = Type~. , geom = "line",
      ylab = "Number of Steps", main = "Time Series of Average Steps Taken", colour = Type)


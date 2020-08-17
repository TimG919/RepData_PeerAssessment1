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

# Step 4lib
avgDailyPattern <- with(activity,aggregate(steps, by = list(interval),mean, na.rm = TRUE))
colnames(avgDailyPattern) <- c('Interval','AvgSteps')
plot(avgDailyPattern$Interval,avgDailyPattern$AvgSteps,type = "l")




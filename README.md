# Reproducible-Research-Coursera
Course Project 1

Analyzing FitBit Data
By- Sandeep Mahapatra
March, 2016

The data for this assignment was downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

## setting working directory
setwd("C:/Users/D1/Desktop/Study Materials & References/Coursera/Reproducible Research/Week 1/repdata-data-activity")

## Importing the dataset
Activity <- read.csv("activity.csv", header = T, sep=",")

Q1. What is mean total number of steps taken per day?

## Summing up steps by day, creating the histogram for the data and calculating the mean and the median for the data
steps_by_day <- aggregate(steps ~ date, Activity, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
The mean is 1.0766 × 10^4 and the median is 10765.

Q2. What is the average daily activity pattern?

## Calculating the Average daily activity pattern, plotting the average number of steps per day by Interval and finding the Interval with the most average steps
steps_by_interval <- aggregate(steps ~ interval, Activity, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835.

Q3. Impute missing values. Compare imputed to non-imputed data.

Missing data needed to be imputed. Only a simple imputation approach was required for this assignment. Missing values were imputed by inserting the average for each interval. Thus, if interval 10 was missing on 10-02-2012, the average for that interval for all days replaced the NA.

## Imputing the missing values and comparing them with the non-missing data
incomplete <- sum(!complete.cases(Activity))
imputed_activity <- transform(Activity, steps = ifelse(is.na(Activity$steps), steps_by_interval$steps[match(Activity$interval, steps_by_interval$interval)], Activity$steps))

## Zeroes were imputed for 10-01-2012 because it was the first day to fit the rising trend of the data.

imputed_activity[as.character(imputed_activity$date) == "2012-10-01", 1] <- 0

## Recounting the total steps by day
steps_by_day_i <- aggregate(steps ~ date, imputed_activity, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

## Creating the Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)

## Calculating the new mean and median for the imputed data
rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)

## Calculating the difference between imputed and non imputed data
mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian

## Calculating the total difference
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)

The imputed data mean is 1.059 × 104
The imputed data median is 1.0766 × 104
The difference between the non-imputed mean and imputed mean is -176.4949
The difference between the non-imputed mean and imputed mean is 1.1887
The difference between total number of steps between imputed and non-imputed data is 7.5363 × 104. Thus, there were 7.5363 × 104 more steps in the imputed data.

Q4. Are there differences in activity patterns between weekdays and weekends?

## Creating a plot to compare the difference between the activity patterns of Weekdays and Weekends
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
imputed_activity$dow <- as.factor(ifelse(is.element(weekdays(as.Date(imputed_activity$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_activity, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

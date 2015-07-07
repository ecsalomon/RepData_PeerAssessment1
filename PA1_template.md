# Reproducible Research: Peer Assessment 1

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web site.

The variables included in this dataset are:

+ **steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA)
+ **date:** The date on which the measurement was taken in YYYY-MM-DD format
+ **interval:** Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment

This report  answers the questions detailed below. 

### Loading and preprocessing the data

Show any code that is needed to:

1. **Load the data (i.e. read.csv())**


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

2. **Process/transform the data (if necessary) into a format suitable for your analysis**

Convert `date` to Date format


```r
activity$date <- as.Date(activity$date)
```

The intervals are stored HHMM format, meaning that they are not evenly spaced when the variable is interpreted as a numeric (i.e., there are gaps between XX55s and YY00s). So convert them to minutes past midnight.


```r
# Start with the minutes past the hour for each interval
activity$minute <- activity[activity$interval < 100,]$interval

# Get the hours
activity$hour <- as.numeric(gsub(".{1,2}$", "", activity$interval))
activity$hour[is.na(activity$hour)]  <- 0

# Minutes past midnight = (minutes past the hour) + (60 * hour)
activity$time <- activity$minute + (60 * activity$hour)
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. **Calculate the total number of steps taken per day**


```r
library(plyr)
stepsPerDay <- ddply(activity, 
                     .(date), 
                     summarize,
                     numSteps = sum(steps, na.rm = TRUE))
```

2. **If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day**


```r
hist(stepsPerDay$numSteps,
     main = "Total Number of Steps Taken per Day",
     xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

3. **Calculate and report the mean and median of the total number of steps taken per day**


```r
meanSteps <- mean(stepsPerDay$numSteps)
medSteps  <- median(stepsPerDay$numSteps)
```

The mean total number of steps taken per day is **9354.23**.  
The median total number of steps taken per day is **10395**.


### What is the average daily activity pattern?

1. **Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

First, create a data set that contains the average steps taken for each interval (as minutes past midnight).


```r
stepsPerInterval <- ddply(activity,
                          .(time, interval), 
                          summarize,
                          avgSteps = mean(steps, na.rm = TRUE))
```

Finally, plot the time series.


```r
with(stepsPerInterval, plot(time, avgSteps, type = "l", 
                            main = "Time Series of Average Steps per Interval",
                            ylab = "Average number of steps",
                            xlab = "Minutes past midnight"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

2. **Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

Sort the data in descending order by average number of steps. The interval from the first row is the one with the maximum number of steps.


```r
stepsPerInterval <- stepsPerInterval[order(stepsPerInterval$avgSteps, 
                                           decreasing = TRUE),] 
```

The interval with maximum number of steps, on average across all days in the dataset, is **835**.


### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


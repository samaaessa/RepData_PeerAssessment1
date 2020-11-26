---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
# load all packages used in this exploratory analysis
library(knitr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data


```r
# load data
data_row <- read.csv('/home/samaaessa/Desktop/DS coursera/course 5/repdata_data_activity/activity.csv')
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day


```r
# remove NA in data
data <- data_row[ with (data_row, { !(is.na(steps)) } ), ]

# grouping data by date
by_day <- group_by(data, date)
steps_by_day <- summarise(by_day, total = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# plotting histogram
hist(steps_by_day$total, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![](Reproducible-Research--Peer-Assessment-1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#calculating the mean and medians
print(paste('mean : ',mean(steps_by_day$total)))
```

```
## [1] "mean :  10766.1886792453"
```

```r
print(paste('median : ',median(steps_by_day$total)))
```

```
## [1] "median :  10765"
```


## What is the average daily activity pattern?
    
1.Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# preprocessing data for plot
steps_by_interval <- aggregate(steps ~ interval, data,mean)

# create a time series plot 
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![](Reproducible-Research--Peer-Assessment-1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# find row with max of steps
max_steps_row <- which.max(steps_by_interval$steps)

# find interval with this max
print("5-minute interval, on average across all the days : ")
```

```
## [1] "5-minute interval, on average across all the days : "
```

```r
steps_by_interval[max_steps_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
print(paste(" numbers of missing values : ",sum(is.na(data_row))))
```

```
## [1] " numbers of missing values :  2304"
```

I picked the strategy of replacing NA’s with the mean for that 5-minute interval.


```r
data_imputed <- data_row

# calculate  total number of steps taken each day
df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)

hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")
```

![](Reproducible-Research--Peer-Assessment-1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

** calculating the mean and medians of imputed data **

```r
print(paste('mean : ',mean(df_imputed_steps_by_day$steps)))
```

```
## [1] "mean :  10766.1886792453"
```

```r
print(paste('median : ',median(df_imputed_steps_by_day$steps)))
```

```
## [1] "median :  10765"
```

** calculating the mean and medians of data without NA's **

```r
print(paste('mean : ',mean(steps_by_day$total)))
```

```
## [1] "mean :  10766.1886792453"
```

```r
print(paste('median : ',median(steps_by_day$total)))
```

```
## [1] "median :  10765"
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2.Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
    

```r
data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"

# convert type_of_day from character to factor
data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

# calculate average steps by interval across all days
df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)

# creat a plot
qplot(interval, steps, data = df_imputed_steps_by_interval, type = 'l', geom=c("line"), xlab = "Interval", ylab = "Number of steps", main = "") +
  facet_wrap(~ type_of_day, ncol = 1)
```

```
## Warning: Ignoring unknown parameters: type
```

![](Reproducible-Research--Peer-Assessment-1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



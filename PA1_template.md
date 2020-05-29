---
title: "PA1_template"
author: "Beth"
date: "5/27/2020"
output: 
  html_document: 
    keep_md: yes
---

In this report, I will be analyzing the number of steps taken throughout the day, ultimately resulting in the comparision of steps taken during the weekend vs the weekday. 

## Loading and preprocessing the data

First, I am loading the data using read.csv. I want to make sure that I don't run  into issues with analysis later, so I am only keeping the lines that don't have NAs. Most lines have complete data.


```r
setwd("~/Desktop/JHU R/Reproducable Research")
activity <- read.csv("activity.csv")
dim(activity)
```

```
## [1] 17568     3
```

```r
ComActivity <- activity[complete.cases(activity), ] 
dim(ComActivity)
```

```
## [1] 15264     3
```

## What is mean total number of steps taken per day?

Using the complete.cases data, I am looking at the average number of steps taken on  each day analyzed. I have produced a histogram of this data.


```r
dayTotals <- aggregate(steps ~ date, ComActivity, sum)
hist(dayTotals$steps, breaks = 20, xlab = "Total Steps per day", main = "Frequency of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meanSteps <- mean(dayTotals$steps)
medianSteps <- median(dayTotals$steps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps
```

```
## [1] 10765
```

The average number of steps is 1.0766189\times 10^{4} and median number is 10765.

## What is the average daily activity pattern?

Below is a graph of the average  steps across each 5 minute interval.


```r
TimeAvg <- aggregate(steps ~ interval, ComActivity, mean)
plot.ts(TimeAvg$interval, TimeAvg$steps, type = "l", xlab  = "Time Interval", ylab = "Average Steps", main = "Average Steps Through Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxNum <- TimeAvg[TimeAvg$steps == max(TimeAvg$steps), ]
maxNum$interval
```

```
## [1] 835
```

The Time Interval that contains the  maximum number of steps, 206.1698113, is 835. 

## Imputing missing values

I have decided to use the average number of steps for each interval period to replace missing values. 


```r
numMiss <- (length(activity$steps)-length(ComActivity$steps)) #could have done sum(is.na(activity$steps))
# Imputing missing values by taking average for the interval
# Making sure date and interview columns are filled in
sum(is.na(activity$date))
```

```
## [1] 0
```

```r
sum(is.na(activity$interval))
```

```
## [1] 0
```

```r
copyActivity <- activity
# for each  line of data
for (x in 1:length(copyActivity$steps)) {
        #check to see if the steps value is NA
        if (is.na(copyActivity[x, ]$steps)) {
                fillVal  <- "test"
                # find average value for the time interval
                testInterval <- copyActivity[x,]$interval
                for(i in 1:length(TimeAvg$interval)) {
                        if (as.numeric(testInterval) == as.numeric(TimeAvg[i,]$interval)) {
                                fillVal <- as.numeric(TimeAvg[i,]$steps)
                        }
                }
                # fill in  value
                copyActivity [x, 1] <- fillVal
        }
}
# check if any NAs now
sum(is.na(copyActivity$steps))
```

```
## [1] 0
```

```r
impDayTotals <- aggregate(steps ~ date, copyActivity, sum)
hist(impDayTotals$steps, breaks = 20, xlab = "Total Steps per day", main = "Frequency of Steps for Imupted")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
impMeanSteps <- mean(impDayTotals$steps)
impMedianSteps <- median(impDayTotals$steps)
impMeanSteps
```

```
## [1] 10766.19
```

```r
meanSteps
```

```
## [1] 10766.19
```

```r
impMedianSteps
```

```
## [1] 10766.19
```

```r
medianSteps
```

```
## [1] 10765
```

```r
# Imputing the values doesn't  change the mean and median that much; probably because it is  a small fraction
```

There were 2304 NA values that I replaced using the average of steps for a given interval. 
The imputed mean, 1.0766189\times 10^{4} is very similar to the mean removing NAs, 1.0766189\times 10^{4}. Same with the imputed median, 1.0766189\times 10^{4}, and NA removed median, 10765.

## Are there differences in activity patterns between weekdays and weekends?

After classifying each day tested as a weekday or weekend, I plot a time series plot to compare the number of steps between the days.


```r
dayendActivity <- ComActivity
dayendActivity$dayend <- factor("weekend", levels = c("weekend", "weekday"))
class(dayendActivity$dayend)
```

```
## [1] "factor"
```

```r
sum(is.na(dayendActivity$dayend))
```

```
## [1] 0
```

```r
dayendActivity$date <- as.Date(dayendActivity$date, format = "%Y-%m-%d")
for(i in 1: length(dayendActivity$date)) {
        if(weekdays(dayendActivity[i, ]$date) %in% c("Sunday", "Saturday")) {
                dayendActivity[i, ]$dayend <- as.factor("weekend")
        } else dayendActivity[i, ]$dayend <- as.factor("weekday")
}
table(dayendActivity$dayend)
```

```
## 
## weekend weekday 
##    4032   11232
```

```r
DayActivity <- subset(dayendActivity, dayend=="weekday")
EndActivity <- subset(dayendActivity, dayend=="weekend")

DayTimeAvg <- aggregate(steps ~ interval, DayActivity, mean)
EndTimeAvg <- aggregate(steps ~ interval, EndActivity, mean)

par(mfrow = c(2, 1))
plot.ts(DayTimeAvg$interval, DayTimeAvg$steps, type = "l", xlab  = "Time Interval", ylab = "Average Steps", main = "Average Steps Through Weekday")
plot.ts(EndTimeAvg$interval, EndTimeAvg$steps, type = "l", xlab  = "Time Interval", ylab = "Average Steps", main = "Average Steps Through Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The patterns are actually quite similar between the weekdays and weekends, with the exception that this individual  seems to sleep in on the weekends. I do too!

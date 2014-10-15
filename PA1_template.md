---
output: html_document
---
---
Reproducible Research: Peer Assessment 1
========================================
<!--  With help from Fabian Linzberger (thanks Fabian :-) - this is how I work and follow 
the same principles for this course. -->


### Loading and preprocessing the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
steps.date <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

2. Calculate and report the **mean** and **median** total number of
   steps taken per day


```r
meanTotalStepsPerDay <- mean(steps.date$steps)
medianTotalStepsPerDay <- median(steps.date$steps)
```
Report: The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> and the median total number of steps per day is 10765.  
\  
\  

## What is the average daily activity pattern?

3. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)


```r
steps.interval <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(steps.interval, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

4. Which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?


```r
max5MinIntSteps <- steps.interval$interval[which.max(steps.interval$steps)]
```
Report: The 5-minute interval with the maximium number of steps is the 835 th  
\  
\  

## Input missing values

5. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sumNAs <- sum(is.na(activity))
```
Report: The total number of missing values are 2304.


6. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

Strategy: Use the means for the 5-minute intervals for missing values.

7. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
activity <- merge(activity, steps.interval, by="interval", suffixes=c("",".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[,c(1:3)]
```

8. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
steps.date <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
meanTotalStepsPerDayNAsFilledIn <- mean(steps.date$steps)
medianTotalStepsPerDayNAsFilledIn <- median(steps.date$steps)
```
Report: The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>
and the median total number of steps per day is 1.0766 &times; 10<sup>4</sup>.

Observation: The impact of the missing data seems rather low, at least when
estimating the total number of steps per day.  
\  
\  


## Are there differences in activity patterns between weekdays and weekends?

9. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

10. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval,
                            data=activity,
                            subset=activity$daytype==type,
                            FUN=mean)
    plot(steps.type, type="l", main=type)
}
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

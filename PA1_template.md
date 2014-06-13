# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load libraries, set system language:


```r
library(knitr)
library(ggplot2)
library(plyr)
library(lattice)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
```

```
## [1] "en_US.UTF-8"
```

Load the data


```r
unzip("activity.zip")
activity <- read.csv(unzip("activity.zip", list = TRUE)$Name)
```

Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$steps <- as.numeric(activity$steps)
# activity$date <- as.POSIXlt(activity$date)
activity$interval <- as.numeric(activity$interval)
```

## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day (ignore the missing values in the dataset)


```r
dailySteps <- with(na.omit(activity), aggregate(steps, by=list(date=date), FUN=sum))
names(dailySteps) <- revalue(names(dailySteps), c("x"="steps"))

g1 <- ggplot(dailySteps, aes(x=as.factor(date),y=steps))
g1 <- g1 + geom_bar(stat="identity", position="dodge")
g1 <- g1 + labs(title="Histogram of steps", x="Day", y="Number of steps")
print(g1)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Calculate and report the mean and median total number of steps taken per day


```r
mean(dailySteps$steps, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(dailySteps$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalSteps  <- with(na.omit(activity), aggregate(steps, by=list(interval=interval), FUN=mean))
names(intervalSteps) <- revalue(names(intervalSteps), c("x"="steps"))

plot(x=intervalSteps$interval, y=intervalSteps$steps , type="l", main="Steps in interval", xlab="Interval", ylab="Average of number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalSteps$interval[which.max(intervalSteps$steps)]
```

```
## [1] 835
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityNoNA <- activity
activityNoNA$steps[is.na(activity$steps)] <- intervalSteps$steps[match(activity$interval[is.na(activity$steps)], intervalSteps$interval)]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
dailyStepsNoNA <- with(activityNoNA, aggregate(steps, by=list(date=date), FUN=sum))
names(dailyStepsNoNA) <- revalue(names(dailyStepsNoNA), c("x"="steps"))

g2 <- ggplot(dailyStepsNoNA, aes(x=as.factor(date),y=steps))
g2 <- g2 + geom_bar(stat="identity", position="dodge")
g2 <- g2 + labs(title="Histogram of steps (with Imputation)", x="Day", y="Number of steps")
print(g2)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

Calculate and report the mean and median total number of steps taken per day


```r
mean(dailyStepsNoNA$steps)
```

```
## [1] 10766
```

```r
median(dailyStepsNoNA$steps)
```

```
## [1] 10766
```

Due to the nature of my imputation, the mean did not change - also my median hardly changed.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityNoNA$day <- as.factor(ifelse(  weekdays.POSIXt(as.POSIXlt(activityNoNA$date)) %in% c("Saturday","Sunday")  ,  "weekend"  ,  "weekday"  ))
intervalStepsNoNA <- with(activityNoNA, aggregate(steps, by=list(interval=interval,day=day), FUN=mean))
names(intervalStepsNoNA) <- revalue(names(intervalStepsNoNA), c("x"="steps"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
xyplot(steps ~ interval | day, intervalStepsNoNA, type="l", layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r
# Alternative:
# g3 <- ggplot(intervalStepsNoNA )
# g3 <- g3 + facet_grid(day~.)
# g3 <- g3 + geom_line(aes(x=as.factor(interval),y=steps))
# g3 <- g3 + labs(title="Steps in interval", x="Interval", y="Number of steps")
# print(g3)
```



# Reproducible Research: Peer Assessment 1



## Set working directory and load libraries

```r
library(knitr)
setwd("~/DSU/datasciencecoursera/reproducibleresearch/RepData_PeerAssessment1")
```


## Loading and preprocessing the data
1. Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
unzip("activity.zip")
rawdata <- read.csv("activity.csv", header=TRUE)
df <- na.omit(rawdata)
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

```r
steps_per_day <- aggregate(steps ~ date, df, sum)
hist(steps_per_day$steps, xlab="Total Steps per Day", main="Histogram of Total Number of Steps per Day")
```

![](figure/unnamed-chunk-3-1.png)<!-- -->

```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$step)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_per_interval <- aggregate(steps ~ interval, df, mean)
plot(steps_per_interval, type="l", xlab="5-min Intervals", ylab="Number of Steps", main="Average Number of Steps Across All Days" )
```

![](figure/unnamed-chunk-4-1.png)<!-- -->

```r
# interval with maximum number of steps
steps_per_interval[which.max(steps_per_interval$steps), ]$interval
```

```
## [1] 835
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# 1. number of rows with NAs
sum(is.na(rawdata$steps))
```

```
## [1] 2304
```

```r
# 2. Fill in missing values with mean for that 5-minute interval
newDF <- rawdata
for(i in 1:nrow(newDF)) {
    if(is.na(newDF$steps[i])) {
        # find mean value for corresponding 5-minute interval and substitute avg steps for the NA
        avg <- steps_per_interval$steps[which(steps_per_interval$interval == newDF$interval[i])]
        newDF$steps[i] <- avg
    }
}
imputeDF <- aggregate(steps ~ date, newDF, sum)
# 4. 
hist(imputeDF$steps, xlab="Total Steps per Day", main="Histogram of Total Number of Steps per Day (Imputed)")
```

![](figure/unnamed-chunk-5-1.png)<!-- -->

```r
mean(imputeDF$steps)
```

```
## [1] 10766.19
```

```r
median(imputeDF$steps)
```

```
## [1] 10766.19
```
For question 4, imputting the missing data does not affect the mean, but the median moves closer to, and now appears to equal the mean value. 

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
df$date <- as.Date(df$date) # convert factors to dates
df$type_of_day <- factor(ifelse(weekdays(df$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"), levels=c("weekday", "weekend"))
average_steps_interval_weekday <- aggregate(steps ~ interval + type_of_day, df, mean)
library(lattice)
xyplot(steps ~ interval | factor(type_of_day), 
       data = average_steps_interval_weekday,
       layout = c(1,2),
       xlab = "Interval",
       ylab = "Number of steps",
       type = "l")
```

![](figure/unnamed-chunk-6-1.png)<!-- -->

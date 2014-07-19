# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

* 1. Load the data

```r
library(plyr)
data <- read.csv("activity.csv", header = TRUE)
data$date <- as.Date(data$date, format="%Y-%m-%d")
data$interval <- as.numeric(data$interval)
```
* 2. Process/transform the data (if necessary) into a format suitable for your analysis 

```r
# Remove NA values for steps
data_no_na <- data[!is.na(data$steps),]
```


## What is mean total number of steps taken per day?

* 1. Make a histogram of the total number of steps taken each day

```r
num_steps_each_day <- ddply(data_no_na, .(date), summarise, sum=sum(steps))
hist(num_steps_each_day$sum, main = 'the total number of steps taken each day', xlab='number of steps')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

* 2. Calculate and report the mean and median total number of steps taken per day

```r
mean_num_steps_per_day<-mean(num_steps_each_day$sum)
mean_num_steps_per_day
```

```
## [1] 10766
```

```r
median_num_steps_per_day<-median(num_steps_each_day$sum)
median_num_steps_per_day
```

```
## [1] 10765
```
* The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> steps.
* The median total number of steps taken per day is 10765 steps.


## What is the average daily activity pattern?

* 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean_num_steps_per_interval <- ddply(data_no_na,.(interval),summarise, mean_steps=mean(steps))
plot(mean_num_steps_per_interval$interval, mean_num_steps_per_interval$mean_steps, ylab = "average number of steps taken", xlab = "Interval", type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

* 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_step <- max(mean_num_steps_per_interval$mean_steps)
resultInterval <- mean_num_steps_per_interval[mean_num_steps_per_interval$mean_steps == max_step, 1]
resultInterval
```

```
## [1] 835
```
The 5-minute interval is 835


## Imputing missing values

* 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
total_number_of_na <- nrow(data) - nrow(data_no_na)
total_number_of_na
```

```
## [1] 2304
```
The total number of rows with NAs is 2304

* 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Using mean_num_steps_per_interval to replace NA data 

* 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_filled <- data
for(i in 1:nrow(data_filled)){
if(is.na(data_filled$steps[i])){
        data_filled$steps[i]<-mean_num_steps_per_interval[mean_num_steps_per_interval$interval==data_filled$interval[i],2]
    }
}
head(data_filled)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```
* 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

```r
total_num_steps_per_day <- ddply(data_filled,.(date),summarise, sum=sum(steps))
hist(total_num_steps_per_day$sum, main = 'the total number of steps taken each day', xlab='number of steps')
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
mean_num_steps_per_day_filled<-mean(total_num_steps_per_day$sum)
mean_num_steps_per_day_filled
```

```
## [1] 10766
```

```r
median_num_steps_per_day_filled<-median(total_num_steps_per_day$sum)
median_num_steps_per_day_filled
```

```
## [1] 10766
```
* The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> steps.
* The median total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> steps.
* there is no difference in new mean(missing values filled in) and old mean (with missing values)
* There is slight change in new median(missing values filled in)


## Are there differences in activity patterns between weekdays and weekends?

*  1. Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.

```r
data_filled$wday <- as.factor(ifelse(weekdays( data_filled$date) %in% c("ÐÇÆÚÁù","ÐÇÆÚÈÕ"), "Weekend", "Weekday"))
table(data_filled$wday)
```

```
## 
## Weekday Weekend 
##   12960    4608
```
* 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(lattice)
total_num_steps_per_wday <- ddply(data_filled,.(interval, wday),summarise, mean_steps=mean(steps))
xyplot(mean_steps ~ interval |  
          wday, data = total_num_steps_per_wday, 
       layout = c(1, 2), ylab = "number of steps", xlab = "Interval", type = 'l')
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

---
output: word_document
---
# Peeer assessment 1

## Loading and preprocessing the data

We will read and analyze the data, data comes in csv format

```r
activity <- read.csv("activity.csv")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

##What is mean total number of steps taken per day?

Firstable we will call two libraries, lubridate for a easy date managing and dplyr for summarize and group easily

```r
library(lubridate)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

*1 Calculate the total number of steps taken per day*

```r
sum_steps_day <- group_by(activity, date) %>% summarize(sum_steps=sum(steps, na.rm=TRUE))
```

*2 #Make a histogram of the total number of steps taken each day*
Here we make the Histogram for the number of steps per day

```r
plot1 <- ggplot(sum_steps_day,aes(date,sum_steps))
plot1 + geom_bar(stat="identity", fill="blue") +
        aes(group=1) +
        labs(x="Date", y="Num of Steps", title="Num of Steps per Day" ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_discrete(breaks = sum_steps_day$date[seq(1,61,10)])
```

![plot of chunk plot_steps_day](figure/plot_steps_day-1.png) 


*3 Calculate and report the mean and median of the total number of steps taken per day*


```r
mean_total_steps <- mean(sum_steps_day$sum_steps[sum_steps_day$sum_steps!=0])
median_total_steps <- median(sum_steps_day$sum_steps[sum_steps_day$sum_steps!=0])
```


##What is the average daily activity pattern?

*1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

We have to get the mean summarizing the steps by interval along all the days, we have to factor the interval and then summarize (mean) by interval and the plot it

```r
mean_steps_interval <- group_by(activity, finterval=factor(interval)) %>% summarize(mean_steps=mean(steps, na.rm=TRUE))
plot2 <- ggplot(mean_steps_interval,aes(finterval,mean_steps))
plot2 + geom_line(stat="identity") +
        aes(group=1) +
        labs(x="Interval", y="Mean of Steps", title="Mean Number of Steps per Interval" ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_discrete(breaks=c(0,400,800,1200,1600,2000,2355))
```

![plot of chunk steps_interval](figure/steps_interval-1.png) 

*2 #Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
max_average_steps_interval <- mean_steps_interval$finterval[which.max(mean_steps_interval$mean_steps)]
```
The interval that has the maximun mean of steps is

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```


## Imputing missing values
*1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*

```r
Missing_activity <- sum(is.na(activity$steps))
```
The number of missing values is:

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

*2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

We will use the mean for replace the missing values

*3 Create a new dataset that is equal to the original dataset but with the missing data filled in.*
the new vars contain "_r" wich means rigth values

```r
activity_r <- activity
a <- length(activity$steps)
for (i in 1:a){
        if (is.na(activity_r$steps[i])) {
                activity_interval <- activity_r$interval[i]
                b <- which(mean_steps_interval$finterval==activity_interval)
                activity_r$steps_r[i] <- mean_steps_interval$mean_steps[b]
        } else{
                activity_r$steps_r[i] <- activity_r$steps[i]
        }
}
```


*4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*
Firstable we will plot the new Histogram

```r
sum_steps_day_r <- group_by(activity_r, date) %>% summarize(sum_steps_r=sum(steps_r, na.rm=TRUE))
library(ggplot2)
plot3 <- ggplot(sum_steps_day_r,aes(date,sum_steps_r))
plot3 + geom_bar(stat="identity", fill="blue") +
        aes(group=1) +
        labs(x="Date", y="Num of Steps", title="Num of Steps per Day" ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_discrete(breaks = sum_steps_day$date[seq(1,61,10)])
```

![plot of chunk plot_his_r](figure/plot_his_r-1.png) 

Now we will calculate mean and median for the fixed data

```r
mean_total_steps_r <- mean(sum_steps_day_r$sum_steps_r[sum_steps_day$sum_steps!=0])
median_total_steps_r <- median(sum_steps_day_r$sum_steps_r[sum_steps_day$sum_steps!=0])
```
The porcentual difference for the mean is

```r
var_mean=(mean_total_steps_r/mean_total_steps-1)*100
print(paste(var_mean,"%"))
```

```
## [1] "0 %"
```
The porcentual difference for the median is

```r
var_median <- (median_total_steps_r/median_total_steps-1)*100
print(paste(var_median,"%"))
```

```
## [1] "0 %"
```

##Are there differences in activity patterns between weekdays and weekends?

*1 Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*

Firstable we will transform the var date to a rigth date format

```r
activity_r$date2 <- ymd(activity$date)
activity_r$day <- weekdays(activity_r$date2)
```

Now we will asign the labels for weekdays and weekends

```r
activity_r$day2 <- ifelse(activity_r$day=="domingo" | activity_r$day=="sábado","weekend","weekday")
```

*2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data*

With the variable creeated (day2) we will asign a facet for the plot

```r
mean_steps_interval_r <- group_by(activity_r, finterval=factor(interval),fday2=factor(day2)) %>% summarize(mean_steps_r=mean(steps_r, na.rm=TRUE))
plot4 <- ggplot(mean_steps_interval_r,aes(finterval,mean_steps_r))
plot4 + geom_line(stat="identity") +
        facet_wrap(~fday2,nrow=2,ncol=1)  +
        aes(group=1) +
        labs(x="Interval", y="Mean of Steps", title="Mean Number of Steps per Interval" ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_discrete(breaks=c(0,400,800,1200,1600,2000,2355))
```

![plot of chunk plot_week](figure/plot_week-1.png) 

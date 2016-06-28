# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.5
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
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
temp <- tempfile()
download.file("https://github.com/Moorecj87/RepData_PeerAssessment1/raw/master/activity.zip", temp)
dat <- read.csv(unzip(temp, "activity.csv"))

#convert interval to repeat each day

dat$interval <- rep(1:288, length.out = 17568)
```

## What is mean total number of steps taken per day?


```r
dat2 <- aggregate(steps ~ date, dat, FUN = sum)
hist(dat2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean is 10766 and the median is 10765.

## What is the average daily activity pattern?


```r
dat3 <- aggregate(steps ~ interval, dat, mean)

with(dat3, plot(interval, steps, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxsteploc <- which.max(dat3$steps)
maxint <- dat3$interval[maxsteploc]
```

The interval with the maximum average number of steps is 104.

## Imputing missing values


```r
nasum <- sum(is.na(dat$steps))
```

The number of NA values is 2304.


```r
# impute missing data using average per interval

# add average per interval to original dataset

dat4<-dat3
for(i in 1:60){dat4<-rbind(dat4, dat3)}
dat5<-cbind(dat, dat4$steps)

#replace NA with average per interval, remove unneccessary column

dat5$steps[is.na(dat5$steps)] <- dat5$`dat4$steps`[is.na(dat5$steps)]
dat5 <- dat5[,1:3]

dat6 <- aggregate(steps ~ date, dat5, FUN = sum)
hist(dat6$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The means with NA vs without are 10766 and 10766 repectively.  The medians are 10765 and 10766 respectively.

## Are there differences in activity patterns between weekdays and weekends?


```r
dat7 <- mutate(dat5, newdate = weekdays(as.Date(dat5$date)))

weekdaylist <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
dat7$newdate <- factor((dat7$newdate %in% weekdaylist), levels = c(FALSE,TRUE),                              labels = c('weekend','weekday'))

dat7.weekday <- aggregate(steps ~ interval, dat7[dat7$newdate=="weekday",], mean)
dat7.weekend <- aggregate(steps ~ interval, dat7[dat7$newdate=="weekend",], mean)

par(mfcol=(c(1,2)))
plot(dat7.weekday, type = "l", main = "Weekdays")
plot(dat7.weekend, type = "l", main = "Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

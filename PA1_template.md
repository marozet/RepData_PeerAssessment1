# Reproducible Research: Peer Assessment 1
2015-03-14  
###by: marozet

### Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.



### Loading and preprocessing the data
First, I loaded necessary libraries and downloaded the data which is then loaded into **mydata**. (Note: this was done under Windows 8, so no good support of curl method in Knitr). The original file can be found [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

```r
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","repdata_data_activity.zip",mode="wb")
mydata<-read.csv(unz("repdata_data_activity.zip","activity.csv"))
nrow(mydata)
```

```
## [1] 17568
```

Let's look at raw data description:

```r
str(mydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

'Date' columns is converted into date variable

```r
mydata[,2] <- as.Date(mydata[,2]) #convert into Date
```

Now let's look at preprocessed data:

```r
str(mydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(mydata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(mydata)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


### What is mean total number of steps taken per day?
Let's see a histogram of the total number of steps taken each day. In the next step I calculated the mean and meadian.

```r
stepsByDay <- mydata %>% group_by(date) %>% summarize(stepsByDay=sum(steps)) #calculte aggregation by date, I don't remove NA here because they would be replaced by 0. NAs will be replaces when calculating mean and median.
hist(stepsByDay$stepsByDay, col="blue", xlab="Number of steps taken", main="Total steps taken each day")
stepsMean <- mean(stepsByDay$steps,na.rm=TRUE)
stepsMedian <- median(stepsByDay$steps,na.rm=TRUE)
abline(v=stepsMean,lwd=2)
legend("topright",legend=c(paste("mean = ",sprintf(fmt="%.2f",stepsMean)),paste("median = ",stepsMedian)))
```

![](PA1_template_files/figure-html/histogram-1.png) 

Mean total number of steps taken per day is **10766.188679**  
Median total number of steps taken per day is **10765**


### What is the average daily activity pattern?
Let's see a plot of average daily activity.

```r
stepsByInterval <- mydata %>% group_by(interval) %>% summarize(stepsMeanByInterval=mean(steps,na.rm=TRUE)) #calculate aggregation by interval
plot(stepsByInterval$interval,stepsByInterval$stepsMeanByInterval,type="l",main="Average number of steps taken per interval",xlab="5-minute intervals",ylab="Average number of steps taken")
intervalMax <- stepsByInterval[stepsByInterval$steps== max(stepsByInterval$stepsMeanByInterval),1]
maxSteps <- max(stepsByInterval$stepsMeanByInterval)
abline(v=intervalMax,col="red",lwd=2)
legend("topright",legend=c(paste("interval with max = ",sprintf(fmt="%.0f",intervalMax)),paste("Max average steps = ",sprintf(fmt="%.2f",maxSteps))))
```

![](PA1_template_files/figure-html/activity-1.png) 

Interval **835** contains the maximum number of steps. Max average steps in that interval is **206.1698113**.


### Imputing missing values

We can see that we have **2304** rows with missing values.

```r
numNA <-sum(is.na(mydata$steps),na.rm=TRUE)
print(numNA)
```

```
## [1] 2304
```
After a quick look at pairs chart it seems that steps~interval relation is much stronger than steps~days, thus using interval for gruping while imputing seems like a better option. 

```r
pairs(mydata,pch=".")
```

![](PA1_template_files/figure-html/pairs-1.png) 

I imputed missing values with mean for a particular 5-minute interval. As we can see we now do not have missing values. I considered using median statistic, but it produced too many zeros, so eventually I opted for mean statistic and rounded the result.

```r
mydataNAImputed <- mydata
impute.median <- function(x) replace(x, is.na(x), round(mean(x, na.rm = TRUE))) #define function for imputing data
#perform imputing grouped by interval
mydataNAImputed <- ddply(mydataNAImputed, ~ interval, transform, steps = impute.median(steps)) 
summary(mydataNAImputed)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

Let's see a histogram of the total number of steps taken each day using imputed data. 

```r
stepsByDay <- aggregate(steps~date,mydataNAImputed,sum) #aggregate new data by date
hist(stepsByDay$steps, col="blue", xlab="Number of steps taken", main="Total steps taken each day")
stepsMean <- mean(stepsByDay$steps)
stepsMedian <- median(stepsByDay$steps)
abline(v=stepsMean,lwd=2)
legend("topright",legend=c(paste("mean = ",sprintf(fmt="%.2f",stepsMean)),paste("median = ",stepsMedian)))
```

![](PA1_template_files/figure-html/histogramOnImputed-1.png) 

Mean total number of steps taken per day is **10765.639344**  
Median total number of steps taken per day is **10762**

The results are pretty close to what we had previously.

### Are there differences in activity patterns between weekdays and weekends?
First, I created a new factor variable with two levels "weekday" and "weekend".

```r
# addWeekDay() - function that will choose between "weekday" and "weekend" based on date
addWeekDay <- function(x) {
    if ((weekdays(x)=="Saturday")|(weekdays(x)=="Sunday")) return("weekend")
    else return("weekday")
}
mydataNAImputed$weekday <- sapply(mydataNAImputed$date,addWeekDay) #apply to a new column
mydataNAImputed[,'weekday']<-as.factor(mydataNAImputed[,'weekday']) #convert to factor
str(mydataNAImputed) #check result
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  2 0 0 47 0 0 0 2 0 34 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-02" ...
##  $ interval: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ weekday : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 2 2 1 1 1 ...
```

Now let's look at the plot containing a time series of the 5-minute interval and the average number of steps taken across all weekday and weekend days.


```r
library(lattice)
stepsByInterval <- aggregate(steps~interval+weekday,mydataNAImputed,mean) #calculate aggregation by interval and weekday
xyplot(steps~interval | weekday, data = stepsByInterval,type="l",main="Average number of steps taken per interval",xlab="5-minute intervals",ylab="Average number of steps taken",layout=c(1,2))
```

![](PA1_template_files/figure-html/time_series_plot-1.png) 

We can see a clear difference between weekends and weekdays. Weekdays data has a very distinct peek between 800 and 900 interval. Then there are less steps on average. In weekend data steps appear to be distributed more evenly.

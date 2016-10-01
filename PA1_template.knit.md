---
title: "Week 2 Assignment"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Activity Monitoring Data

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis
```{r}
setwd("C:/Users/Mandy/Documents/R/Coursera/5. Reproducible Research/Week 2")
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day



```{r}
activitysum <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)

```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
```{r}
hist(activitysum, xlab = "Total Daily Steps", main = "Total Number of Steps Taken Each Day")
```



3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
activitymean <- tapply(activity$steps, activity$date, mean, na.rm=TRUE)
activitymedian <- tapply(activity$steps, activity$date, median, na.rm=FALSE)
activitysummary <- cbind(activitymean, activitymedian)
colnames(activitysummary) <- c("Mean", "Median")
activitysummary
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```{r}
activityintervalssum <- aggregate(activity$steps, by=list(Category=activity$interval), FUN=sum, na.rm=TRUE)
plot(activityintervalssum$Category, activityintervalssum$x, type="l", xlab = "Interval", ylab= "Steps", col="green" , lwd=2)
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
activityintervalssumsrt <- activityintervalssum[order(-activityintervalssum$x),]
activityintervalssumsrt[1,]

```


### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```{r}
summary(activity)
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```{r}
activitymeanimput <- aggregate(activity$steps, by=list(category=activity$interval), FUN=mean, na.rm=TRUE)
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```{r}


colnames(activitymeanimput) <- c("Interval", "Ave Steps")
activitynew <- activity
activityna <- subset(activitynew, is.na(activitynew$steps))
activityna <- cbind(activityna, activitymeanimput)
activitynew <- merge(activitynew, activityna, all = TRUE)
activitynew$newsteps <- ifelse (!is.na(activitynew$`Ave Steps`),  activitynew$`Ave Steps`, activitynew$steps)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}

activitynewsum <- tapply(activitynew$newsteps, activitynew$date, sum)
hist(activitynewsum, xlab = "Total Daily Steps", main = "Total Number of Steps Taken Each Day (imputted)")
activitynewmean <- tapply(activitynew$newsteps, activitynew$date, mean)
activitynewmedian <- tapply(activitynew$newsteps, activitynew$date, median)
activitynewsummary <- cbind(activitynewmean, activitynewmedian)
colnames(activitynewsummary) <- c("Mean", "Median")
activitynewsummary

```

### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1, Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```{r}
Sys.setlocale("LC_TIME", "English")
activitynew$weekday <- weekdays(activitynew$date)
activitynew$weekend <- ifelse(activitynew$weekday=="Saturday" | activitynew$weekday == "Sunday",1,0)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}

activitynewssum <- aggregate(activitynew$newsteps, by=list(Category=activitynew$interval,activitynew$weekend), FUN=sum, na.rm=TRUE)
colnames(activitynewssum) <- c("Interval","Weekend","Steps")
par(mfrow = c(2, 1))
par(mar=c(0.5,0.5,0.5,0.5))
with(subset(activitynewssum, Weekend==0),plot(Interval, Steps, type = "l", main = "Weekdays"))
with(subset(activitynewssum, Weekend==1),plot(Interval, Steps, type = "l", main = "Weekends"))            

```





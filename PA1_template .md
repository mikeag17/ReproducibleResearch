---
title: "ReproducibleResearch"
date: "6/10/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Load and Process Data
```{r echo = TRUE}
activity<-read.csv("activity.csv")
head(activity)
```
# 1 Calculate  and report total number of steps per day
```{r echo = TRUE}

activity_filter<- activity[!is.na(activity$steps),]

daily_steps<-aggregate(steps~date,activity_filter,sum)
daily_steps

```

# 2 Histogram of the total number of steps taken each day

```{r echo = TRUE}
hist(as.numeric(daily_steps$steps),xlab = "Daily Steps",main = "Total Number of steps taken each day")

```


# 3 mean and mediam numberof steps taken each day
```{r echo = TRUE}

mean(daily_steps$steps)

median((daily_steps$steps))

```

# 4 Time series plot of the average number of steps taken



```{r echo = TRUE}

interval_agg<-aggregate(steps~interval,activity_filter,mean)

plot(interval_agg$interval,interval_agg$steps,type="l", xlab = "Interval", ylab = "Average steps")

```

# 5 The 5-minute interval that, on average, contains the maximum number of steps
```{r echo = TRUE}

interval_agg$interval[which.max(interval_agg$steps)]
interval_agg$steps[which.max(interval_agg$steps)]
```
Interval 835 contains the highest number of steps with 206.1698 steps



# 6 Code to describe and show a strategy for imputing missing data

 First we must identify which rows have NA values.
 

```{r echo = TRUE}
na_idx<-is.na(activity$steps)
head(na_idx)
```
Next we can calculate the average number of steps per interval to substitute for each row with NA
```{r echo = TRUE}
steps<-activity$steps
mean(steps,na.rm = TRUE)
```
We can now replace all the missing values with the above average
```{r echo = TRUE}
new_activity<-activity

new_activity[na_idx,1]<-37.3826
```

All missing values have now been replaced


# 7 Histogram of the total number of steps taken each day after missing values are imputed


```{r echo = TRUE}

filled_data<- aggregate(steps~date,new_activity,sum)
hist(as.numeric(filled_data$steps),xlab = "Daily Steps(replaced data)",main = "Total Number of steps taken each day")
```


# 8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r echo = TRUE}

dw<-weekdays(as.Date(new_activity$date,'%Y-%m-%d'))


```

```{r echo = TRUE}
new_activity[,2]<-dw
idx = new_activity[,2] == "Saturday" | new_activity[,2]=="Sunday"
weekend <- new_activity[idx==TRUE,]
weekdays<- new_activity[idx==FALSE,]

weekend_agg<- aggregate(steps~interval,weekend,sum)
weekday_agg<- aggregate(steps~interval,weekdays,sum)


par(mfrow=c(2,1))
plot(weekend_agg$interval,weekend_agg$steps,type = "l", xlab = "Interval",ylab = "Weekend Steps")
plot(weekday_agg$interval,weekday_agg$steps,type = "l", xlab = "Interval",ylab = "Weekday Steps")

```


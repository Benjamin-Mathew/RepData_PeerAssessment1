---
title: "Reproducible Research PA1"
author: "Benjamin Mathew"
date: "November 10, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

## Assignment

### Loading libraries

```{r}
library("plyr")  
library("dplyr")  
library("lubridate")  
library("reshape2")  
library("ggplot2")
```

### Loading and Preprocessing the data

1. Load the data (i.e. read.csv())

```{r}
data_row <- read.csv('activity.csv')
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
data <- data_row[ with (data_row, { !(is.na(steps)) } ), ]
```


### What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

2. Histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
by_day <- group_by(data, date)
steps_by_day <- summarise(by_day, total = sum(steps))
steps_by_day
hist(steps_by_day$total, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
summary(steps_by_day)
```

Mean of total number of steps per day is 10766, median is 10765.

### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
## preprocessing data for plot
steps_by_interval <- aggregate(steps ~ interval, data, mean)

## create a time series plot 
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
## find row with max of steps
max_steps_row <- which.max(steps_by_interval$steps)

## find interval with this max
steps_by_interval[max_steps_row, ]
```

The interval 835 has the maximum average value of steps (206.1698).

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
sum(is.na(data_row))
```

Total number of rows with NA's is 2304.

Strategy:
```{r}
data_imputed <- data_row
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- steps_by_interval[
      steps_by_interval$interval == interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}
```

New Data set:
```{r}
## calculate  total number of steps taken each day
df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
head(df_imputed_steps_by_day)
```

Histogram:
```{r}
hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")

## get mean and median of imputed data
mean(df_imputed_steps_by_day$steps)

median(df_imputed_steps_by_day$steps)

## get mean and median of data without NA's
mean(steps_by_day$total)

median(steps_by_day$total)
```

Mean values stays the same but there is slight difference in meadian value.

### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```{r}
data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"

## Create a plot
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = data_imputed, subset = data_imputed$type_of_day == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}

```

```{r, include=FALSE}
   # add this chunk to end of mycode.rmd
   file.rename(from="PA1_template.Rmd", 
               to="PA1_template.md")
```

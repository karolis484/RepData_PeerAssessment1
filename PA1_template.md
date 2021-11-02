---
title: " PA1_template"
author: "Karolis"
date: "11/1/2021"
output: html_document
---

Loading required libraries
```{r,echo = TRUE}
library(ggplot2)
library(knitr)
library(dplyr)
```


1) Code for reading in the data set and processing the data:

```{r,echo = TRUE}

#importing
raw_data <- read.csv("activity.csv", header = TRUE)

#removing NA
my_data <- raw_data[ with (raw_data, { !(is.na(steps)) } ), ]

#setting as date
my_data$date <- as.Date(as.character(my_data$date), c("%Y-%m-%d"))


```

2) Histogram of the total number of steps taken each day

```{r,echo = TRUE}

#calculating the sum of each day
steps_sum <- aggregate(steps ~ date,my_data,sum)

hist(steps_sum$steps,main ="Number of steps per day",xlab="Number of steps",ylab="Frequency, days")

```

3a) Median number of steps taken each day:

```{r,echo = TRUE}
median(steps_sum$steps)

```
3b) Mean of steps taken each day is:
```{r,echo = TRUE}
mean(steps_sum$steps)
```

4) Time series plot of the average number of steps taken:

```{r,echo = TRUE}

#calculating tre mean for each interval
steps_by_interval <- aggregate(steps ~ interval, my_data, mean)

plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval, 5 min", 
     ylab="Average number of steps")


```

5) The 5-minute interval that, on average, contains the maximum number of steps:

```{r,echo = TRUE}

steps_by_interval[which.max(steps_by_interval$steps), ]

```


6) Code to describe and show a strategy for imputing missing data:

a) Total number of missing values in the dataset:

```{r,echo = TRUE}

sum(is.na(raw_data))

```

b) changing the missing values to the mean for that 5-minute interval
```{r,echo = TRUE}

#replacing NAs with the mean of 5 min interval
my_data_noNA <- raw_data
for (i in 1:nrow(my_data_noNA)) {
  if (is.na(my_data_noNA$steps[i])) {
    interval_value <- my_data_noNA$interval[i]
    steps_value <- steps_by_interval[
      steps_by_interval$interval == interval_value,]
    my_data_noNA$steps[i] <- steps_value$steps
  }
}

```

7) Histogram of the total number of steps taken each day after missing values are imputed:

```{r,echo = TRUE}

#calculating the total number of steps
steps_sum_noNA <- aggregate(steps ~ date,my_data_noNA,sum)

hist(steps_sum_noNA$steps,main ="Number of steps per day",xlab="Number of steps",ylab="Frequency, days")

```

Median number of steps taken each day with NA is:

```{r,echo = TRUE}

median(steps_sum$steps)

```

Median number of steps taken each day without NA is:

```{r,echo = TRUE}

median(steps_sum_noNA$steps)

```


Mean of steps taken each day with NA is:
```{r,echo = TRUE}

mean(steps_sum$steps)

```


Mean of steps taken each day without NA is:
```{r,echo = TRUE}

mean(steps_sum_noNA$steps)

```
Median is slightly different, while mean is the same.


8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

a) Making a list of weekdays and weekends

```{r,echo = TRUE}

#making a list of weekdays and weekends
my_data_noNA['type_of_day'] <-weekdays(as.Date(my_data_noNA$date))
my_data_noNA$type_of_day[my_data_noNA$type_of_day %in% c('Saturday','Sunday') ] <- "weekend"
my_data_noNA$type_of_day[my_data_noNA$type_of_day !="weekend" ] <- "weekday"


#seting type_of day as factor
my_data_noNA$type_of_day <- as.factor(my_data_noNA$type_of_day)

#calculating the mean
weekday_weekend_mean <- aggregate(steps ~ interval + type_of_day, my_data_noNA, mean)

```

b) drawing a plot

```{r,echo = TRUE}
qplot(data = weekday_weekend_mean, 
      interval, 
      steps, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval, 5 min.", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)
```

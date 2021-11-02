Loading required libraries

    library(ggplot2)
    library(knitr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

1.  Code for reading in the data set and processing the data:

<!-- -->

    #importing
    raw_data <- read.csv("activity.csv", header = TRUE)

    #removing NA
    my_data <- raw_data[ with (raw_data, { !(is.na(steps)) } ), ]

    #setting as date
    my_data$date <- as.Date(as.character(my_data$date), c("%Y-%m-%d"))

1.  Histogram of the total number of steps taken each day

<!-- -->

    #calculating the sum of each day
    steps_sum <- aggregate(steps ~ date,my_data,sum)

    hist(steps_sum$steps,main ="Number of steps per day",xlab="Number of steps",ylab="Frequency, days")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

3a) Median number of steps taken each day:

    median(steps_sum$steps)

    ## [1] 10765

3b) Mean of steps taken each day is:

    mean(steps_sum$steps)

    ## [1] 10766.19

1.  Time series plot of the average number of steps taken:

<!-- -->

    #calculating tre mean for each interval
    steps_by_interval <- aggregate(steps ~ interval, my_data, mean)

    plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
         main="Average number of steps over all days", xlab="Interval, 5 min", 
         ylab="Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

1.  The 5-minute interval that, on average, contains the maximum number
    of steps:

<!-- -->

    steps_by_interval[which.max(steps_by_interval$steps), ]

    ##     interval    steps
    ## 104      835 206.1698

1.  Code to describe and show a strategy for imputing missing data:

<!-- -->

1.  Total number of missing values in the dataset:

<!-- -->

    sum(is.na(raw_data))

    ## [1] 2304

1.  changing the missing values to the mean for that 5-minute interval

<!-- -->

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

1.  Histogram of the total number of steps taken each day after missing
    values are imputed:

<!-- -->

    #calculating the total number of steps
    steps_sum_noNA <- aggregate(steps ~ date,my_data_noNA,sum)

    hist(steps_sum_noNA$steps,main ="Number of steps per day",xlab="Number of steps",ylab="Frequency, days")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Median number of steps taken each day with NA is:

    median(steps_sum$steps)

    ## [1] 10765

Median number of steps taken each day without NA is:

    median(steps_sum_noNA$steps)

    ## [1] 10766.19

Mean of steps taken each day with NA is:

    mean(steps_sum$steps)

    ## [1] 10766.19

Mean of steps taken each day without NA is:

    mean(steps_sum_noNA$steps)

    ## [1] 10766.19

Median is slightly different, while mean is the same.

1.  Panel plot comparing the average number of steps taken per 5-minute
    interval across weekdays and weekends

<!-- -->

1.  Making a list of weekdays and weekends

<!-- -->

    #making a list of weekdays and weekends
    my_data_noNA['type_of_day'] <-weekdays(as.Date(my_data_noNA$date))
    my_data_noNA$type_of_day[my_data_noNA$type_of_day %in% c('Saturday','Sunday') ] <- "weekend"
    my_data_noNA$type_of_day[my_data_noNA$type_of_day !="weekend" ] <- "weekday"


    #seting type_of day as factor
    my_data_noNA$type_of_day <- as.factor(my_data_noNA$type_of_day)

    #calculating the mean
    weekday_weekend_mean <- aggregate(steps ~ interval + type_of_day, my_data_noNA, mean)

1.  drawing a plot

<!-- -->

    qplot(data = weekday_weekend_mean, 
          interval, 
          steps, 
          type = 'l', 
          geom=c("line"),
          xlab = "Interval, 5 min.", 
          ylab = "Number of steps", 
          main = "") +
      facet_wrap(~ type_of_day, ncol = 1)

    ## Warning: Ignoring unknown parameters: type

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-16-1.png)

---
title: "Reproducible Research Project 1"
author: "Jake Willis"
date: "1/20/2022"
output: md_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Loading and preprocessing the data
## Show any code that is needed to 
1. Load the data (i.e. <span style="color:red"> read.csv()</span>)
2. Process/transform the data (if necessary) into a format suitable for your analysis
```{r, message = FALSE, error = FALSE, warning = FALSE, echo = TRUE}

#Set working directory to location you desire to download the data.
setwd("~/Military/COURSERA_OReilly/COURSERA")

# Clear the Global Environment.
rm(list = ls())

# Install necessary libraries.
library(dplyr)          # A set of functions designed to enable data frame manipulation in a intuitive, user-friendly way.
library(kableExtra)     # A package to help you build common complex tables and manipulate table styles.
library(ggplot2)        # An open-source data visualization package.
library(lubridate)      # Provides tools that make it easier to parse and manipulate dates.

# Create value for the location of the data.
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Download the compressed file and unzip it.
download.file(url, "repdata_data_activity.zip")
unzip("repdata_data_activity.zip")

# Load the activity monitoring devices dataset into a dataframe.
activity_data <- read.csv("activity.csv", header = TRUE)

# Change the class of the "date" variable from factor to date.
activity_data$date <- as.Date(levels(activity_data$date)[activity_data$date], format = "%Y-%m-%d")
```

# What is the mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the dataset.
1. Calculate the total number of steps taken per day.
2. Make a histogram of the total number of steps taken each day.
3. Calculate and report the mean and median of the total number of steps taken per day.
```{r, message = FALSE, error = FALSE, warning = FALSE, echo = TRUE}

# Create a data frame with the total number of steps taken per day.
daily_steps <- activity_data %>%
  group_by(date) %>%
  summarize(Total_Steps = sum(steps))

# Display the total number of steps taken per day.
kbl(daily_steps) %>%
  kable_paper %>%
  scroll_box(width = "500px", height = "200px")

# Make a histogram of the total number of steps taken each day.
hist(daily_steps$Total_Steps, col = "violet", main = "Histogram of the Total Number of Steps taken each day", xlab = "Total Number of Steps", breaks = 15)

# Calculate and report the mean and median of the total number of steps taken per day to two decimal places.
daily_steps_mean <- round(mean(daily_steps$Total_Steps, na.rm = TRUE), digits = 0)
daily_steps_median <- round(median(daily_steps$Total_Steps, na.rm = TRUE), digits = 0)

print(paste0("For the total number steps taken per day, the mean is ", daily_steps_mean, " and the median is ", daily_steps_median, "."))
```

# What is the average daily activity pattern?
1. Make a time series plot (i.e. <span style="color:red"> type = "l"</span>) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, message = FALSE, error = FALSE, warning = FALSE, echo = TRUE}

# Create a data frame with the total number of steps taken per 5-minute interval.
interval_steps <- activity_data %>%
  group_by(interval) %>%
  summarize(Total_Steps = sum(steps, na.rm = TRUE))

# Calculate the average number of steps taken per 5-minute interval across all the days.
interval_steps$Average_Steps <- interval_steps$Total_Steps / length(daily_steps$date)

# Calculate the average number of steps taken, averaged across all days.
daily_interval_mean <- round(mean(activity_data$steps, na.rm = TRUE), digits = 0)

# Find index for interval with max steps on average across all the days in the dataset.
max_step_index <- which.max(interval_steps$Average_Steps)
max_step_interval_x <- interval_steps[max_step_index, ]$interval
max_step_interval_y <- floor(interval_steps[max_step_index, ]$Average_Steps)

# Create a dataframe with the index results of the max steps.
max_step_data <- data.frame()

# Create a time series plot of the 5-minute interval and average number of steps taken, averaged across all days and show the interval with the max steps.
ggplot(data = interval_steps, aes(x = interval, y = Average_Steps)) +
  geom_line() +
  geom_point(aes(x = max_step_interval_x, y = max_step_interval_y), colour = "orange", size = 3) +
  geom_text(aes(x = max_step_interval_x, y = max_step_interval_y + 10), label = "Max Steps", color = "orange", size = 4) +
  labs(x = "5-minute Interval", y = "Average Number of Steps", title ="Average Number of Steps in 5-minute intervals")

# Report the maximum steps on average across all the days taken during a 5-minute interval and the time at which it occurred.
print(paste0("For the average number steps taken per day, the max steps occurred during the 5-minute interval of ", max_step_interval_x, " and the number of steps is approximately ", max_step_interval_y, "."))
```

# Imputing missing values
## Note that there are a number of days/intervals where there are missing avlues (coded as <span style="color:red"> NA</span>). The presence of missing days may introduce bias into some calculations or summaries of the data.
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with <span style="color:red"> NA</span>s).
2. Devise a strategy filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r, message = FALSE, error = FALSE, warning = FALSE, echo = TRUE}

# Determine the number of "NA" entries in each variable.
total_NA <- sum(is.na(activity_data))

# Report the the total number of missing values in the dataset.
print(paste0("There are ", total_NA, " rows with missing values in the dataset."))

# Join the activity data with the average steps in a 5-minute interval.
activity_data2 <- left_join(activity_data, interval_steps, by = "interval")

# Conditionally, replace the NAs in the step variable with the average steps in the 5-minute interval.
activity_data2 <- activity_data2 %>%
                    mutate(steps = coalesce(steps, Average_Steps))

# Create a data frame with the total number of steps taken per day.
daily_steps <- activity_data2 %>%
  group_by(date) %>%
  summarize(Total_Steps = sum(steps))

# Display the total number of steps taken per day.
kbl(daily_steps) %>%
  kable_paper %>%
  scroll_box(width = "500px", height = "200px")

# Make a histogram of the total number of steps taken each day.
hist(daily_steps$Total_Steps, col = "violet", main = "Histogram of the Total Number of Steps taken each day", xlab = "Total Number of Steps", breaks = 15)

# Calculate and report the mean and median of the total number of steps taken per day to two decimal places.
daily_steps_mean <- round(mean(daily_steps$Total_Steps, na.rm = TRUE), digits = 0)
daily_steps_median <- round(median(daily_steps$Total_Steps, na.rm = TRUE), digits = 0)

print(paste0("For the total number steps taken per day, the mean is ", daily_steps_mean, " and the median is ", daily_steps_median, "."))

print(paste0("When we replace the intervals missing values with the average steps taken in that interval, we notice a difference in values for the mean and median total number of steps taken per day. The mean drops from 10,766 to 10,581 steps. Additionally, the median drops from 10,765 to 10,395 steps."))
```

# Are there differences in activity patterns between weekdays and weekends?
## For this part of the <span style="color:red"> weekdays()</span> function may be of some help here. Use the dataset with the filled-in missing values for this part.
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. <span style="color:red"> type = "l"</span>) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays days or weekend days (y-axis).
```{r, message = FALSE, error = FALSE, warning = FALSE, echo = TRUE}

# Create a new factor variable in the dataset with two levels to annotate day of the week.
activity_data2$week <- ifelse(weekdays(activity_data2$date) %in%
                                c("Saturday", "Sunday"), "weekend", "weekday")

# Create a data frame with the total number of steps taken per 5-minute interval.
interval_steps <- activity_data2 %>%
  group_by(interval, week) %>%
  summarize(Total_Steps = sum(steps, na.rm = TRUE))

# Calculate the average number of steps taken per 5-minute interval across all the days.
interval_steps$Average_Steps <- interval_steps$Total_Steps / length(daily_steps$date)

# Make a panel plot cotaining a time series plot.
ggplot(data = interval_steps, aes(x = interval, y = Average_Steps)) +
  geom_line() +
  facet_wrap(vars(week), nrow = 2) +
  labs(x = "Interval", y = "Number of Steps", title ="Average Number of Steps in 5-minute intervals")

```

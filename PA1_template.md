---
title: "Activity Monitoring Data Analysis"
date: "2025-03-17"
output: html_document
---

## **Loading and Preprocessing the Data**


``` r
library(dplyr)
library(ggplot2)

zip_file <- "repdata_data_activity.zip"
csv_file <- "activity.csv"
if (!file.exists(csv_file)) {
  unzip(zip_file)
}

activity_data <- read.csv(csv_file, stringsAsFactors = FALSE)

activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")

head(activity_data)
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

## **Total Number of Steps Taken Per Day**


``` r
daily_steps <- activity_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

ggplot(daily_steps, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill="blue", color="black", alpha=0.7) +
  labs(title = "Histogram of Total Steps Per Day", x = "Total Steps", y = "Frequency") +
  theme_minimal()
```

![plot of chunk total_steps_per_day](figure/total_steps_per_day-1.png)


``` r
# Compute mean and median
mean_steps <- mean(daily_steps$total_steps, na.rm = TRUE)
median_steps <- median(daily_steps$total_steps, na.rm = TRUE)

# Print results
mean_steps
```

```
## [1] 9354.23
```

``` r
median_steps
```

```
## [1] 10395
```

## **Average Daily Activity Pattern**


``` r
interval_avg <- activity_data %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

ggplot(interval_avg, aes(x = interval, y = avg_steps)) +
  geom_line(color="red") +
  labs(title = "Average Daily Activity Pattern", x = "5-Minute Interval", y = "Average Steps") +
  theme_minimal()
```

![plot of chunk time_series_plot](figure/time_series_plot-1.png)


``` r
max_interval <- interval_avg %>%
  filter(avg_steps == max(avg_steps))

max_interval
```

```
## # A tibble: 1 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```

## **Imputing Missing Values**


``` r
total_missing <- sum(is.na(activity_data$steps))
total_missing
```

```
## [1] 2304
```


``` r
interval_means <- activity_data %>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE))

activity_imputed <- activity_data %>%
  left_join(interval_means, by="interval") %>%
  mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
  select(-mean_steps)
```


``` r
daily_steps_imputed <- activity_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

ggplot(daily_steps_imputed, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill="green", color="black", alpha=0.7) +
  labs(title = "Histogram of Total Steps Per Day (Imputed Data)", x = "Total Steps", y = "Frequency") +
  theme_minimal()
```

![plot of chunk histogram_imputed](figure/histogram_imputed-1.png)

## **Weekday vs. Weekend Activity**


``` r
activity_imputed <- activity_imputed %>%
  mutate(day_type = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
activity_imputed$day_type <- factor(activity_imputed$day_type, levels = c("Weekday", "Weekend"))
```


``` r
interval_avg_daytype <- activity_imputed %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

``` r
ggplot(interval_avg_daytype, aes(x = interval, y = avg_steps, color=day_type)) +
  geom_line() +
  facet_wrap(~day_type, ncol=1) +
  labs(title = "Activity Pattern (Weekday vs. Weekend)", x = "5-Minute Interval", y = "Average Steps") +
  theme_minimal()
```

![plot of chunk weekday_vs_weekend_plot](figure/weekday_vs_weekend_plot-1.png)



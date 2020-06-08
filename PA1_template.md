---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Introduction

Thisassignment is a brief analysis of the steps performed by an individual over two months measured at 5 minute intervals.

## Loading and preprocessing the data


```r
#Read the data & call libraries
if(!file.exists("activity.csv")){
unzip("activity.zip")  
}
basedata = read.csv("activity.csv")
library(dplyr)
library(ggplot2)
```



```r
# Data transformation, total steps by date
newbase = basedata %>%
  group_by(date)%>%
  summarise(stepsbydate = sum(steps))
```

## What is mean total number of steps taken per day?

The average daily steps is 10,765, the histogram shows a distribution close to normal.



```r
summary(newbase$stepsbydate)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```



```r
ggplot(newbase, aes(stepsbydate))+
  geom_histogram(aes(y=..density..))+
  geom_density(col="red")+
  labs(title = "Histogram of daily steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is the average daily activity pattern?

The graph and the table show a very marked peak in the 5-minute interval that corresponds to the time from 1:55 p.m. to 2:00 p.m., in fact, the following intervals in the list are also close to this time. During this mentioned period, on average each day this individual performs 206 steps.


```r
MeanSteps = basedata  %>%
  filter(is.na(steps) == FALSE)%>%
  group_by(interval)%>%
  summarise(Mean = round(mean(steps),0))

ggplot(MeanSteps, aes(x=interval, y=Mean))+
  geom_line()+
  labs(title="Average for each 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->






```r
knitr::kable(head(arrange(MeanSteps, desc(Mean)),n = 10), align = 'c',
             caption = "Top ten of steps in 5 minutes intervals")
```



Table: Top ten of steps in 5 minutes intervals

 interval    Mean 
----------  ------
   835       206  
   840       196  
   850       183  
   845       180  
   830       177  
   820       171  
   855       167  
   815       158  
   825       155  
   900       143  


## Imputing missing values

The _'missing values'_ will be replaced by the average values for the corresponding five-minute interval, this is achieved first by calculating said average, and then substitute each missing value in the base.


```r
# Calculate the average for each 5-minute interval

basedataNA = basedata %>%
  group_by(interval)%>%
  mutate(newsteps = mean(steps, na.rm=TRUE))

# This part replaces the NAs by the average of the corresponding five-minute interval.

for (i in 1:length(basedataNA$newsteps)) {
  if (is.na(basedataNA$steps[i])==TRUE) {
    basedataNA$steps[i] <- basedataNA$newsteps[i]
  }
  
}
```



### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

In this case, the distribution is clearly more focused on the mean and in effect there is no difference between the mean and the median.


```r
newbaseNA = basedataNA %>%
  group_by(date)%>%
  summarise(stepsbydate = sum(steps))

summary(newbaseNA$stepsbydate)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
ggplot(newbaseNA, aes(stepsbydate))+
  geom_histogram(aes(y=..density..))+
  geom_density(col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

The graph shows the average differences from weekends. During Saturday and Sunday the morning activity clearly decreases and on the other hand, in the daytime periods there is more activity on the weekends.


```r
basedata$date <- as.Date(basedata$date)
basedata$DoW <- weekdays(basedata$date)
basedata$WeekEnd = "No"
basedata$WeekEnd[basedata$DoW == "sábado" | basedata$DoW=="domingo"] = "Yes"

MeanStepsNA = basedata  %>%
  filter(is.na(steps) == FALSE)%>%
  group_by(interval, WeekEnd)%>%
  summarise(Mean = mean(steps))

ggplot(MeanStepsNA, aes(x=interval, y=Mean))+
  geom_line(aes(col=WeekEnd))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

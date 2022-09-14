---
title: "reproducible research"
author: "Siri"
date: "2022-09-10"
output: html_document
---


```{r}
setwd("C:/Users/siric/Desktop/R_programming")
```

```{r}
unzip("./activity.zip")
activityData <- read.csv("./activity.csv")
summary(activityData)
```

```{r}
names(activityData)
```




```{r}
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
```

```{r}
hist(stepsPerDay$steps)
```

```{r}
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay
```

```{r}
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay
```

```{r}
stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")
```

```{r}
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
```

```{r}
totalValuesMissings <- sum(is.na(activityData$steps))
totalValuesMissings
```

```{r}
getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}
```

```{r}
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
    }
}
```

```{r}
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
```

```{r}
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
```

```{r}
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
```

```{r}
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```
 

---
title: "pa1_reprodres_coursera"
output: html_document
---


What is mean total number of steps taken per day?
--------------------------------------------------------------------------------

set up environment (libraries, working directory etc.)
and read the data

```{r, echo=TRUE}
library(ggplot2)
library(reshape)
library(reshape2)
library(gridExtra)

mywd<-"d:\\R_school\\coursera\\reproducible_research\\assessment1\\"
setwd(mywd)
dat<-read.csv(".\\data\\activity.csv")
```

1. Make a histogram of the total number of steps taken each day (ignore NA)

```{r, echo=TRUE}
molted   <- melt(dat, id.vars = "date", measure.vars = "steps")
daymeans <- cast(molted, date~variable,mean) 
qplot(daymeans$steps, geom="histogram",binwidth = 3)
```

2. Calculate and report the mean and median total number of steps taken per day

```{r, echo=TRUE}
paste("mean:",mean(daymeans$steps, na.rm=T))
paste("median:",median(daymeans$steps, na.rm=T))
```

Imputing missing values
--------------------------------------------------------------------------------
1. Calculate and report the total number of missing values in the dataset 

```{r}
sum(is.na(dat$steps))
```

2.3.&4. Strategy for filling in all of the missing values in the dataset based on 
        interval means. Create a new data set without NAs, make a histogramm and
        report median and mean

```{r, echo=TRUE}
minterval     <- melt(dat, id.vars = "interval", measure.vars = "steps")
intervalmeans <- dcast(minterval, interval~variable,fun.aggregate = mean, 
                       na.rm = TRUE)

nona_dat <- dat 
for (i in 1: length(intervalmeans$interval)){
        nona_dat$steps[which(is.na(dat$steps) & dat$interval == 
                intervalmeans$interval[i])] <-intervalmeans$steps[i]              
} 
nona_molted    <- melt(nona_dat, id.vars = "date", measure.vars = "steps")
nona_daymeans <- cast(nona_molted, date~variable,mean) 
qplot(nona_daymeans$steps, geom="histogram",binwidth = 3)

paste("mean:",mean(nona_daymeans$steps))
paste("median:",median(nona_daymeans$steps))
```
Do these values differ from the estimates from the first part of the assignment?


Yes it does!

What is the impact of imputing missing data on the estimates of the total daily 
number of steps?


no difference between the *mean* and the *median* after replacing NAs
before:
```{r}
mean(daymeans$steps, na.rm= T)- median(daymeans$steps, na.rm=T)
```
which seems small since unit is *steps* however


after the replacement:  

```{r}
mean(nona_daymeans$steps)- median(nona_daymeans$steps)
```


**Thus, the effect might be negligible in this case.**




Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------------

get English weekdays independent from OS setup and then subset

```{r, echo=TRUE}
nona_dat$wd <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
  "Friday", "Saturday")[as.POSIXlt(nona_dat$date)$wd+1]

week_dat    <- subset(nona_dat, nona_dat$wd != c("Saturday","Sunday"))
weekend_dat <- subset(nona_dat, nona_dat$wd == c("Saturday","Sunday"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


get the mean
```{r, echo=TRUE}
melt_week_interval    <- melt(week_dat,id= "interval", measure.vars = "steps")
melt_weekend_interval <- melt(weekend_dat,id= "interval", measure.vars = "steps")

mean_week   <- dcast(melt_week_interval, interval~variable,fun.aggregate = mean)
mean_weekend<- dcast(melt_weekend_interval, interval~variable,fun.aggregate = mean)
```


3. 5-minute interval that, on average, contains the maximum number of steps.


```{r,echo=TRUE}
melt_interval <- melt(nona_dat,id= "interval", measure.vars = "steps")
mean_interval <- dcast(melt_interval, interval~variable,fun.aggregate = mean)
mean_interval$interval[which(mean_interval$steps == max(mean_interval$steps))]

```



penalplot

```{r, echo=TRUE}

p1<-  ggplot(mean_weekend, aes(x=interval, y=steps)) +
      geom_line(colour="blue") + 
      #plot.margin= unit(c(1, 1, 1, 1))+
      xlab("Interval")+ylab("Number of steps")+
      ggtitle("Weekday")


p2<-  ggplot(mean_week, aes(x=interval, y=steps)) +
      geom_line(colour="blue") + 
      #plot.margin= unit(c(1, 1, 1, 1))+
      xlab("Interval")+ylab("Number of steps")+
      ggtitle("Weekend")

gp1 <- ggplot_gtable(ggplot_build(p1))
gp2 <- ggplot_gtable(ggplot_build(p2))
maxWidth = grid::unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- as.list(maxWidth)
gp2$widths[2:3] <- as.list(maxWidth)
grid.arrange(gp1, gp2, ncol=1)
```






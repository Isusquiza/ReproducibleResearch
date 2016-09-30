## 1. Loading and preprocessing the data

##Establecer dirección del directorio
setwd("G:/RespaldoEdgar/SDSH/2016 DPB/Solicitudes/R PRUEBA/Coursera/5. Reproducible Research")

##Bajar los archivos 
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile ="./ActivityData.zip")
unzip('./ActivityData.zip', exdir = './ActivityData')

##Llamar los datos
activity <- read.csv("./ActivityData/activity.csv")
dim(activity)
head(activity)

##Cálculo de pasos por día
library(dplyr)
step.day <- summarise(group_by(activity, date), steps=sum(steps)) 

## 2.Histogram of the total number of steps taken each day 
hist(step.day$steps, main="Histogram of steps taken each day", col="green", xlab="Steps", breaks=20)

## 3.Mean and median number of steps taken each day
total.day1 <- summarise(group_by(activity, date), sum.steps=sum(steps))
mean(total.day1$sum.steps, na.rm=TRUE)
median(total.day1$sum.steps, na.rm=TRUE)

## 4.Time series plot of the average number of steps taken
library(ggplot2)
step.mean.interval <- summarise(group_by(activity, interval), mean.steps=mean(steps, na.rm=TRUE))
g <- ggplot(step.mean.interval, aes(x=interval, y=mean.steps, colour="red"))
g + geom_line()+labs(title="Time serie of 5 min interval and the average of steps taken")+labs(x="5-minute interval")+labs(y="average steps")

## 5. Calculating Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max(step.mean.interval$mean.steps)
max.step <- which.max(step.mean.interval$mean.steps)
step.mean.interval$interval[max.step]

## 6. Imputing Missing Values
## 6.1 Total number of missing value
sum(is.na(activity$steps))

## 6. 2 Replace missing values
activity.na <- data.frame(activity, dlsteps= is.na(activity$steps))
al <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE, simplify=T)
activity.na$steps[activity.na$dlsteps] <- al[as.character(activity.na$interval[activity.na$dlsteps])]

## 7.Time series plot of the average number of steps taken with replace data
step.day2 <- summarise(group_by(activity.na, date), steps=sum(steps)) 

##Make a histogram
hist(step.day2$steps, main="Histogram of steps taken each day, with replace in missing data", col="green", xlab="Steps", breaks=20)

## 7.2 Calculate mean and median with replace data
total.day2 <- summarise(group_by(activity.na, date), sum.steps=sum(steps))
mean(total.day2$sum.steps, na.rm=TRUE)
median(total.day2$sum.steps, na.rm=TRUE)

##8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

days <- weekdays(activity.na[,2])
activity.na <- cbind(activity.na,days)
library(plyr)
activity.na$days <- revalue(activity.na$days,c("lunes"="weekday","martes"="weekday","miércoles"="weekday","jueves"="weekday","viernes"="weekday"))
activity.na$days <- revalue(activity.na$days,c("sábado"="weekend","domingo"="weekend"))

## Panel Plot
wk_activity <- aggregate(steps ~ days+interval, data=activity.na, FUN=mean)

##library(lattice)
xyplot(steps ~ interval | factor(days),
       layout = c(1, 2),
        xlab="Interval",
       main="Time Series of Average of Total Steps (weekend and weekdays)",
      ylab="Number of steps",
       type="l",
       lty=1,
       data=wk_activity)




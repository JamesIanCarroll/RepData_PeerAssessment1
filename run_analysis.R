#General Set-up
setwd("D:\\Dropbox\\Coursera\\Reproducible Research\\Project 1")
unzip("D:\\Dropbox\\Coursera\\Reproducible Research\\Project 1\\repdata-data-activity.zip")
activity  <- read.csv("activity.csv", header = TRUE, stringsAsFactors=FALSE, na.strings="NA")

#Make Proper Date Column
activity$date <- as.Date(activity$date)

#Removing Missing Values
activityNoNA <- activity[!is.na(activity$steps),]

#Question1
TotalPerDay <- tapply(activityNoNA$steps, activityNoNA$date, sum, na.rm=TRUE, simplify=T)

#Plot the Graph
png("TotalStepsPerDay.png")
Q1Graph <- hist(x=TotalPerDay, col="grey", breaks=20, xlab="Total Steps Per Day", ylab="Frequency", main="Frequency Counts for the Number of Steps Per Day")
dev.off()

#Calculate Mean and Median
TotalMean <- mean(TotalPerDay)
TotalMedian <- median(TotalPerDay)

#Print the Means and Medians
TotalMean
TotalMedian

#Question2
#Table the Interval Averages
IntervalAverage <- tapply(activityNoNA$steps, activityNoNA$interval, mean, na.rm=TRUE, simplify=T)
#Turn the results into a dataframe
IntervalAverage <- data.frame(IntervalAverage)
#Populate a column with the row names (Interval Label)
IntervalAverage$Interval <- row.names(IntervalAverage)
#Show an example of the new dataframe
head(IntervalAverage)

#Graph the Averages
png("IntervalAverages.png")
Q2Graph <- plot(y=IntervalAverage$IntervalAverage, x=IntervalAverage$Interval, type="l", ylab="Average Number of Steps", xlab="Interval", main="Average Number of Steps Per Interval")
dev.off()

#Maximum Number of Average Steps
MaxSteps <- max(IntervalAverage$IntervalAverage)

#Which interval has this?
MaxStepsInterval <- IntervalAverage[IntervalAverage$IntervalAverage == MaxSteps,]

#Print Max Steps Interval and the Maximum Number of Steps
MaxStepsInterval$Interval
MaxStepsInterval$IntervalAverage

#Question 3 Inputting Missing Values
#Number of rows with NA's
#could also use sum(!is.na(activity))
CompleteRows <- complete.cases(activity)
NumberIncompleteRows <- length(CompleteRows[CompleteRows==FALSE])

#Filling empty cells
#Create a new table
TestingDataframe <- activity
#Set a subset of NA's in the steps column
StepsNA <- is.na(TestingDataframe$steps)
#Recalculate the Average Number of Steps across Intervals
IntervalAverage <- tapply(activityNoNA$steps, activityNoNA$interval, mean, na.rm=TRUE, simplify=T)
#Replace the subset of NA's with the appropriate Interval Average
TestingDataframe$steps[StepsNA] <- IntervalAverage[as.character(TestingDataframe$interval[StepsNA])]

#Repeat Q1 with filled empty cells
TotalPerDayNoNA <- tapply(TestingDataframe$steps, TestingDataframe$date, sum, na.rm=TRUE, simplify=T)

#Plot graph
png("FrequencyCountPerDay.png")
Q3Graph <- hist(x=TotalPerDayNoNA, col="grey", breaks=20, xlab="Total Steps Per Day", ylab="Frequency", main="Frequency Counts for the Number of Steps Per Day")
dev.off()

#Calculate new mean and median
TotalMeanNoNA <- mean(TotalPerDayNoNA)
TotalMedianNoNA <- median(TotalPerDayNoNA)

#Print new mean and median
TotalMeanNoNA
TotalMedianNoNA

#Find the Differences
TotalMean-TotalMeanNoNA
TotalMedian-TotalMedianNoNA

#Put in weekdays
TestingDataframe$Weekday <- weekdays(TestingDataframe$date)


#Choose Weekday or Weekend
TestingDataframe$Workingday <- TestingDataframe$Weekday
TestingDataframe$Workingday <- gsub("Monday", "Weekday", TestingDataframe$Workingday)
TestingDataframe$Workingday <- gsub("Tuesday", "Weekday", TestingDataframe$Workingday)
TestingDataframe$Workingday <- gsub("Wednesday", "Weekday", TestingDataframe$Workingday)
TestingDataframe$Workingday <- gsub("Thursday", "Weekday", TestingDataframe$Workingday)
TestingDataframe$Workingday <- gsub("Friday", "Weekday", TestingDataframe$Workingday)
TestingDataframe$Workingday <- gsub("Saturday", "Weekend", TestingDataframe$Workingday)
TestingDataframe$Workingday <- gsub("Sunday", "Weekend", TestingDataframe$Workingday)

#Aggregate the number of steps based on working day and interval then calculate mean.
ToGraph <- aggregate(steps~Workingday+interval, data=TestingDataframe, FUN=mean)

#Load the right package
library(ggplot2)

#Plot Graph
graph4   <- ggplot(ToGraph, aes(x=interval, y=steps, group=1)) + geom_line()
graph4a  <- graph6 + facet_grid(Workingday~.) + ylab("Average Number of Steps per Interval") + xlab("5 Minute Interval") + ggtitle("Average Number of Steps per 5-Minute Interval during Weekdays and Weekends")
print(graph4a)
ggsave(filename="WeekdayWeekend.png", plot=graph4a)



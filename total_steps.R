library(data.table)
library(ggplot2)

# Set the working directory to the correct path
setwd("C:/Users/Hawking's/Desktop/datasciencecoursera/Reproducible reasearch")

# Read the CSV file into a data frame
data <- data.table::fread(input = "activity.csv")


# Now you can work with the 'data' data frame
head(data)  # View the first few rows of the data

# avg steps per day
total_steps<- data[, vapply(.SD,sum,na.rm=FALSE,numeric(1)), .SDcols = "steps", by=.(date)]

#converting steps column into numeric
total_steps$steps<- as.numeric(total_steps$steps)

#mean and median of total steps per day
mm_steps<-c(mean=mean(total_steps$V1, na.rm=TRUE), median=median(total_steps$V1,na.rm=TRUE))

# Plotting avg steps
png("Plot_avg_steps.png",width=500,height=500)

ggplot(total_steps, aes(x= V1))+ geom_histogram(fill="blue", binwidth = 1000)+labs(title="Daily steps", x="steps", y="frequency")

dev.off()


#avg daily activity pattern
interval_data<- data[, vapply(.SD,mean,na.rm=TRUE,numeric(1)),.SDcols="steps",by=.(interval)]

head(interval_data)

#plotting steps in intervals
png("plot_interval_steps.png",width=500,height=500)

ggplot(interval_data, aes(x=interval, y= V1))+geom_line(color="blue",size=1)+labs(title="Avg. daily steps",x="Intervals",y="Avg. steps")

dev.off()

#interval with max avg steps

max_steps<- interval_data[V1==max(V1), .(max_interval=interval)]

print(max_steps)

# calculating NA values
nrow(data[is.na(steps),])

#Handling Na values
data[is.na(steps),"steps"]<-data[,c(lapply(.SD,median,na.rm=TRUE)),.SDcols=c("steps")]

# total number of steps taken per day
Total_Steps <- data[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 



# mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

#plotting
png("Plot daily steps(NA_rem).png", width = 500, height=500)

ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")

dev.off()

# Just recreating data from scratch then making the new factor variable. (No need to, just want to be clear on what the entire process is.) 
data <- data.table::fread(input = "activity.csv")
data[, date := as.POSIXct(date, format = "%Y-%m-%d")]
data[, `Day of Week`:= weekdays(x = date)]
data[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
data[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
data[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(data, 10)

data[is.na(steps), "steps"] <- data[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- data[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

png("Avg. daily steps by weektype.png", width = 500, height=500)

ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)

dev.off()
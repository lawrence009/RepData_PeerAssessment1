#read the data and explore the data

if (!exists('subject')) {
    subject <- read.csv('activity.csv')
}
subject$time <- strptime(paste(subject$date, sprintf('%04d', subject$interval)),
                         '%F%H%M')
subject$date <- as.Date(subject$date)

print(paste('range:', range(subject$steps, na.rm = T)))

str(subject)

total.steps.day <- tapply(subject$steps, subject$date, sum, na.rm = T)
mean.steps.day <- mean(total.steps.day)
median.steps.day <- mean(total.steps.day)

mean.steps.interval <- tapply(subject$steps, subject$interval, mean, na.rm = T)

length(which(is.na(subject$steps)))

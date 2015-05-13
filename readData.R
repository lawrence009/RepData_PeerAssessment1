#read the data and explore the data

if (!exists('subject')) {
    subject <- read.csv('activity.csv')
}
subject$datetime <- strptime(paste(subject$date, sprintf('%04d', subject$interval)),
                         '%F%H%M')
subject$date <- as.Date(subject$date)

print(paste('range:', range(subject$steps, na.rm = T)))

str(subject)

total.steps.day <- tapply(subject$steps, subject$date, sum, na.rm = T)

mean.steps.interval <- tapply(subject$steps, subject$interval, mean, na.rm = T)

length(which(is.na(subject$steps)))

# 0=Sunday, 6=Saturday
is.weekend <- function(wday) {
    if (wday == 0 | wday == 6) {
        TRUE
    } else {
        FALSE
    }
}

wknd <- sapply(subject$datetime$wday, is.weekend)
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

avg.steps.interval <- tapply(subject$steps, subject$interval, mean, na.rm = T)

print(sum(is.na(subject$steps)))

# 0=Sunday, 6=Saturday
is.weekend <- function(wday) {
    if (wday == 0 | wday == 6) {
        TRUE
    } else {
        FALSE
    }
}

subject$wknd <- sapply(subject$datetime$wday, is.weekend)



t.subject <- subject
t.subject$steps[(which(is.na(t.subject)))] <- avg.steps.interval
t.subject$wknd <- factor(t.subject$wknd, levels = c(T, F), labels = c('weekend', 'weekday'))

df <- aggregate(t.subject$steps, by = list(t.subject$interval, t.subject$wknd), mean)
colnames(df) <- c('interval', 'wkd', 'avg.steps')
df$time <- seq(from = 0, by = 5, length.out = 288)
xyplot(avg.steps ~ time | wkd, data = df, type = 'l', layout = c(1, 2))



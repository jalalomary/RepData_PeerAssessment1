library(ggplot2)


read_data <- function() {
  tbl <- read.csv("activity.csv", header=T, colClasses=c("numeric", "character", "numeric"))
  tbl$interval <- factor(tbl$interval)
  tbl$date <- as.Date(tbl$date, format="%Y-%m-%d")
  tbl
}
tbl <- read_data()

steps_per_day <- aggregate(steps ~ date, tbl, sum)
colnames(steps_per_day) <- c("date", "steps")

mean_steps = round(mean(steps_per_day$steps), 2)
median_steps = round(median(steps_per_day$steps), 2)

mean_line <- data.frame(Statitical_Lines="Mean", vals=c(mean_steps))
median_line <- data.frame(Statitical_Lines="Median", vals=c(median_steps))

lines <- rbind(mean_line,median_line)

ggplot(data=steps_per_day, aes(x=steps)) +
  geom_histogram(fill="black", binwidth=1000) +
  geom_vline(data=lines, 
             aes(xintercept=vals, 
                 linetype=Statitical_Lines,
                 colour = Statitical_Lines),
             show_guide = TRUE)+
  ggtitle("Daily total number of steps taken")

calc_steps_per_interval <- function(tbl) {
  steps_pi <- aggregate(tbl$steps, by=list(interval=tbl$interval),
                        FUN=mean, na.rm=T)
  # convert to integers for plotting
  steps_pi$interval <- as.integer(levels(steps_pi$interval)[steps_pi$interval])
  colnames(steps_pi) <- c("interval", "steps")
  steps_pi
}

steps_per_interval <- calc_steps_per_interval(tbl)
max_step_interval <- steps_per_interval[which.max(steps_per_interval$steps),]$interval

cols = c("red")

ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
  geom_line(color="black", size=1) +  
  geom_vline(aes(xintercept=max_step_interval, color="red")) +  
  scale_color_manual(name=element_blank(), values=cols) +     
  labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps")


na_indices <- which(is.na(tbl$steps))
defaults <- steps_per_interval
na_replacements <- unlist(lapply(na_indices, FUN=function(idx){
  interval = tbl[idx,]$interval
  defaults[defaults$interval == interval,]$steps
}))
imp_steps <- tbl$steps
imp_steps[na_indices] <- na_replacements

complete_tbl <- data.frame(  
  steps = imp_steps,  
  date = tbl$date,  
  interval = tbl$interval)

summary(complete_tbl)

complete_steps_per_day <- calc_steps_per_day(complete_tbl)
complete_mean_steps = round(mean(complete_steps_per_day$steps), 2)
complete_median_steps = round(median(complete_steps_per_day$steps), 2)

mean_line <- data.frame(Statitical_Lines="Mean", vals=c(complete_mean_steps))
median_line <- data.frame(Statitical_Lines="Median", vals=c(complete_median_steps))

lines <- rbind(mean_line,median_line)

ggplot(data=complete_steps_per_day, aes(x=steps)) +
  geom_histogram(fill="black", binwidth=1000) +
  geom_vline(data=lines, 
             aes(xintercept=vals, 
                 linetype=Statitical_Lines,
                 colour = Statitical_Lines),
             show_guide = TRUE)+
  ggtitle("Daily total number of steps taken")


tbl$weekday <- as.factor(weekdays(tbl$date))
weekend_data <- subset(tbl, weekday %in% c("Saturday","Sunday"))
weekday_data <- subset(tbl, !weekday %in% c("Saturday","Sunday"))

weekend_spi <- calc_steps_per_interval(weekend_data)
weekday_spi <- calc_steps_per_interval(weekday_data)

weekend_spi$dayofweek <- rep("weekend", nrow(weekend_spi))
weekday_spi$dayofweek <- rep("weekday", nrow(weekday_spi))

day_of_week_data <- rbind(weekend_spi, weekday_spi)
day_of_week_data$dayofweek <- as.factor(day_of_week_data$dayofweek)

ggplot(day_of_week_data, 
       aes(x=interval, y=steps)) + 
  geom_line(color="black", size=1) + 
  facet_wrap(~ dayofweek, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps")



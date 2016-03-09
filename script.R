unzip("activity.zip") # unzip the data zip file
data <- read.csv(file = "activity.csv", header = TRUE, na.strings = "NA") # read the csv file
clean_data <- data[!is.na(data$steps), ] # ignore data rows with NA

library(dplyr) # use dplyr for processing data
steps_by_date <- (clean_data %>%  
                    group_by(date) %>%        # group data by date
                    select(date, steps) %>%   # select only columns date and steps
                    summarise_each(funs(sum)))# summarize the total number of steps by each date

hist(steps_by_date$steps, 
     main = "Histogram of distribution of number of steps for each day", 
     xlab = "number of steps for each day")
dev.copy(png,'./figure/histogram_step_by_date.png') # save figure into file
dev.off()

mean_step <- mean(steps_by_date$steps)
median_step <- median(steps_by_date$steps)
mean_step
median_step

steps_by_interval <- (clean_data %>%                    # from clean_date
                        group_by(interval) %>%          # group data by interval
                        select(interval, steps) %>%     # only keep 2 columns interval and steps
                        summarise_each(funs(mean)) %>%  
                        arrange(interval))              # arrange the result in order of interval

plot(steps_by_interval$interval, steps_by_interval$steps, 
     type = "l", xlab = "interval", ylab = "average number of steps")
dev.copy(png,'./figure/step_by_interval.png') # save figure into file
dev.off()

max_step <- max(steps_by_interval$steps)
max_step_interval <- steps_by_interval[steps_by_interval$steps == max_step, "interval"]
max_step_interval
max_step

n_missing <- sum(is.na(data$steps))
n_missing

new_data <- data
new_data[is.na(new_data$steps), "steps"] <- median_step / (12 * 24)

new_steps_by_date <- (new_data %>% 
                        group_by(date) %>% 
                        select(date, steps) %>% 
                        summarise_each(funs(sum)))

hist(new_steps_by_date$steps, 
     main = "Histogram of distribution of number of steps for each day
     after doing missing value imputation", 
     xlab = "number of steps for each day")
dev.copy(png,'./figure/step_by_date_after_filling_NA.png')
dev.off()

new_mean_step <- mean(new_steps_by_date$steps)
new_median_step <- median(new_steps_by_date$steps)
new_mean_step
new_median_step

new_data$weekday <- factor(ifelse(weekdays(strptime(new_data$date, "%Y-%m-%d")) 
                                  %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                  "weekday", "weekend"))

new_steps_by_interval <- (new_data %>%
                            group_by(interval, weekday) %>%
                            select(interval, weekday, steps) %>%
                            summarise_each(funs(mean)) %>%
                            arrange(interval, weekday))

library(lattice)
xyplot(steps ~ interval | weekday, data = new_steps_by_interval, type = "l", 
       layout = c(1, 2), ylab = "number of steps")
dev.copy(png,'./figure/weekday_vs_weekend.png')
dev.off()

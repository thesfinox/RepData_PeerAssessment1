# Unzip the file with the data
file.zip <- file.path(".", "activity.zip")
file.csv <- file.path(".", "activity.csv")
if(file.exists(file.zip) & !file.exists(file.csv)) {unzip(file.zip)}

# Read the CSV file
data <- read.csv(file.csv, header = TRUE, stringsAsFactors = FALSE)

# Show first few lines
head(data)

# Transform data in columns (transform dates, steps and intervals are fine)
data$date <- as.Date(data$date, "%Y-%m-%d")

# Convert table to data.table for improved manipulation
library(data.table)
data <- data.table(data)

# Compute the total no. of steps per date and plot a histogram
library(ggplot2)
data.steps <- data[,
                   .(steps.sum = sum(steps, na.rm = TRUE),
                     wday = weekdays(date)
                   ),
                   by = date
                  ]

# Compute mean and median of the total no. of steps
data.steps.median <- median(data.steps$steps.sum)
data.steps.mean   <- mean(data.steps$steps.sum)
print(paste("Median:", data.steps.median))
print(paste("Mean:", data.steps.mean))

# Plot the resulting bar plots
plot.steps <- ggplot(data = data.steps, aes(x = date, y = steps.sum, fill = wday)) +
              scale_x_date(breaks = data.steps$date[seq(1, length(data.steps$date), 7)]) +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
              scale_fill_brewer(palette = "YlOrRd") +
              geom_bar(stat = "identity") +
              xlab("date") +
              ylab("total no. of steps (dotted = median, dashed = mean)") +
              guides(fill = guide_legend(title = "weekdays")) +
              geom_hline(yintercept = data.steps.median, linetype = "dotted") +
              geom_hline(yintercept = data.steps.mean, linetype = "dashed") +
              scale_linetype_manual(name = "statistics") +
              ggtitle("Total no. of steps in each day")
#print(plot.steps)

# Compute average no. of steps
data.avg <- data[, .(steps.avg = mean(steps, na.rm = TRUE)), by = interval]

# Plot the time series of the data
plot.avg <- ggplot(data = data.avg, aes(x = interval, y = steps.avg)) +
            geom_step(direction = "hv") +
            xlab("interval") +
            ylab("average no. of steps") +
            ggtitle("Average no. of steps per interval")

#print(plot.avg)

# Find the max no. of steps
max.pos <- which.max(data.avg$steps.avg)
max.val <- data.avg[max.pos,]$interval
print(paste("Max no. of steps (", max.val, ") in the ", max.pos, "th time step", sep = ""))

# Find total no. of missing data
na.steps <- sum(as.numeric(is.na(data$steps)))
na.dates <- sum(as.numeric(is.na(data$date)))
na.intvs <- sum(as.numeric(is.na(data$interval)))
print(paste("Total no. of NA values in: ",
            "step --> ", na.steps, ", ",
            "date --> ", na.dates, ", ",
            "interval --> ", na.intvs, ".",
            sep = ""
           )
     )
print(paste("Total no. of rows with NA values:", dim(data[is.na(data$steps)])[1]))

# Merge data and its average by interval (this way to each interval we associate an avg)
data.byinterval <- merge(data, data.avg, by = "interval")
data.byinterval <- data.byinterval[order(date, interval),]
data.byinterval$steps <- as.double(data.byinterval$steps)
data.byinterval <- data.byinterval[is.na(steps), steps := steps.avg]

# Show the first few lines of the new dataset
head(data.byinterval)

# Recompute the total no. of steps
data.steps.fill <- data.byinterval[,
                                   .(steps.sum = sum(steps, na.rm = TRUE),
                                   wday = weekdays(date)
                                   ),
                                  by = date
                                 ]
# Plot again the total no. of steps
plot.steps.fill <- ggplot(data = data.steps.fill, aes(x = date, y = steps.sum, fill = wday)) +
                          scale_x_date(breaks = data.steps.fill$date[seq(1, length(data.steps.fill$date), 7)]) +
                          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                          scale_fill_brewer(palette = "YlOrRd") +
                          geom_bar(stat = "identity") +
                          xlab("date") +
                          ylab("total no. of steps (dotted = median, dashed = mean)") +
                          guides(fill = guide_legend(title = "weekdays")) +
                          geom_hline(yintercept = data.steps.fill.median, linetype = "dotted") +
                          geom_hline(yintercept = data.steps.fill.mean, linetype = "dashed") +
                          scale_linetype_manual(name = "statistics") +
                          ggtitle("Total no. of steps in each day (NA filled w/ interval mean)")
#print(plot.steps.fill)

# Recompute mean and median of the total no. of steps
data.steps.fill.median <- median(data.steps.fill$steps.sum)
data.steps.fill.mean   <- mean(data.steps.fill$steps.sum)
print(paste("Median:", data.steps.fill.median))
print(paste("Mean:", data.steps.fill.mean))

# Now add a variable to distinguish weekdays and weekends
day.factor <- format(data.byinterval$date, "%u")
day.factor[day.factor %in% 1:5] <- "Weekday"
day.factor[day.factor %in% 6:7] <- "Weekend"
day.factor <- factor(day.factor, levels = c("Weekday", "Weekend"))
data.byinterval$day <- day.factor

# Group by interval and compute the average step divided in weekday and weekend
data.byday <- data.byinterval[,
                              .(steps.avg = mean(steps, na.rm = TRUE)),
                              by = list(interval, day)
                             ]

plot.avg.byday <- ggplot(data = data.byday, aes(x = interval, y = steps.avg)) +
                  geom_step(direction = "hv") +
                  facet_grid(. ~ day) + 
                  xlab("interval") +
                  ylab("average no. of steps") +
                  ggtitle("Average no. of steps per interval")

#print(plot.avg.byday)

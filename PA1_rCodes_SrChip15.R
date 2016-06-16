# Assignment: Course Project 1
# Activity Montioring Data Analysis
# Due Date: June 19, 2016
################################################################################


# download the relevant dataset from an URL source
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(URL, temp)

# load data and remove temp
org.df <- read.csv(unz(temp, "activity.csv"), stringsAsFactors = FALSE)
unlink(temp)

# explore dataset
dim(org.df)
summary(org.df)
str(org.df)

# convert date from char to date and summarize
org.df$date <- as.Date(org.df$date)
agg_date.df <- setNames(aggregate(org.df$steps ~ org.df$date, 
                                  data = org.df, FUN = sum), 
                        c("Date","Steps"))
head(agg_date.df)

# mean and median steps per day
mean_org <- mean(agg_date.df$Steps)
median_org <- median(agg_date.df$Steps)
print(mean_org)
print(median_org)

# histogram of steps per day
library(ggplot2)

ggplot(agg_date.df, aes(agg_date.df$Steps)) +
        geom_histogram(aes(y = ..density..),
                       breaks = seq(0,22000, by = 700),
                       col = "#FF2028",
                       fill = "#00FF43",
                       alpha = 0.2) +
        geom_density(col = 2) +
        xlim(c(min(agg_date.df$Steps), max(agg_date.df$Steps))) +
        labs(title = "Histogram of Daily Steps") + 
        labs(x = "Steps per Day", y = "Frequency")

# averaging the number of steps over 5-minute intervals 
agg_int.df <- setNames(aggregate(org.df$steps ~ org.df$interval, data = org.df, 
                                 FUN = mean), 
                       c("Interval","Steps"))

# 5 minute interval with the maximum number of steps
max_int_avg <- agg_int.df[which.max(agg_int.df$Steps),]
print(max_int_avg)

# time series plot of average steps across intervals
ggplot(agg_int.df, aes(x = Interval, y = Steps)) +
        geom_line(color = "blue") +
        labs(title = "Time Series of Daily Average Steps over Intervals") +
        labs(x = "Intervals", y = "Number of Steps")


# number of missing values
missing_val <- sapply(org.df, function(x) sum(is.na(x)))
print(missing_val)

# imputing missing values
# copy original dataset
imp.df <- org.df

# impute missing values with the mean
imp.df$steps[is.na(imp.df$steps)] <- mean(imp.df$steps, na.rm = TRUE)
str(imp.df)

# aggregate the imputed data set to get the total number of steps taken daily
agg_date_imp.df <- setNames(aggregate(imp.df$steps ~ imp.df$date, 
                                      data = imp.df, FUN = sum), 
                            c("Date","Steps"))

# histogram of total number of steps taken each day (with imputed values)
library(scales)

ggplot(agg_date_imp.df, aes(agg_date_imp.df$Steps)) +
        geom_histogram(aes(y = ..density..),
                       breaks = seq(0,22000, by = 700),
                       col = "#000AB2",
                       fill = "#FFD220",
                       alpha = 0.2) +
        geom_density(col = 2) +
        xlim(c(min(agg_date_imp.df$Steps),max(agg_date_imp.df$Steps))) +
        scale_y_continuous(labels = comma) +
        labs(title = "Histogram of Daily Steps (w/ imputed val)") +
        labs(x = "Steps per Day", y = "Frequency")

# mean and median of imputed dataset
mean_imp <- mean(agg_date_imp.df$Steps)
median_imp <- median(agg_date_imp.df$Steps)
print(mean_imp)
print(median_imp)

# add DayType (weekend/weekday) feature into the imputed dataset
weekend <- c("Saturday","Sunday")
imp.df$DayType <- as.factor(ifelse(weekdays(imp.df$date) %in% weekend, "Weekend", "Weekday"))

# aggregate data points over intervals
agg_int_imp.df <- setNames(aggregate(imp.df$steps ~ imp.df$interval + imp.df$DayType, 
                                     data = imp.df, FUN = mean), 
                           c("Interval","DayType", "Steps"))

# time series panel plot for Interval Mean Steps - Weekday Vs. Weekend
ggplot(agg_int_imp.df, aes(x = Interval, y = Steps, color = DayType)) +
        geom_line() +
        facet_wrap(~DayType, ncol = 1, nrow = 2) +
        labs(title = "Interval Mean Steps - Weekday Vs. Weekend") + 
        labs(x = "Interval", y = "Mean Steps")


################################################################################
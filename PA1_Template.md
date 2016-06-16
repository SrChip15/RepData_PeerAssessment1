# Reproducible Research - Project 1: Activity Monitoring Data Analysis





In this independent study, we will be performing a superficial analysis on the dataset
compiled from the measurement of daily activity monitoring via monitoring devices
such as Fitbit, Nike Fuelband, or Jawbone Up during the months of October and 
November 2012, which we would refer to as *project period* in the below write-up.  


To begin, we are going to:  

*  download the dataset

*  load the dataset into our working space

*  finally, remove the temp file, which we no longer need


```r
# download the relevant dataset from an URL source and load it into workspace
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(URL, temp)
org.df <- read.csv(unz(temp, "activity.csv"), stringsAsFactors = FALSE)
unlink(temp)
```


Now that the dataset has been loaded, let's run some statistics to get an overview


```r
# explore dataset
dim(org.df)
```

```
## [1] 17568     3
```

```r
summary(org.df)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
str(org.df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Right off the bat, we observe that "date" variable is interpreted as a
**character** datatype.  
So, let's go ahead and convert it into the appropriate **date** format.


```r
# convert date from char to date
org.df$date <- as.Date(org.df$date)
str(org.df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
The "str" call now confirms that the said variable is in the appropriate date format


Now, let's **aggregate** the dataset to get the total number of steps taken in a day


```r
#  aggregate number of steps on date
agg_date.df <- setNames(aggregate(org.df$steps ~ org.df$date, 
                                  data = org.df, FUN = sum), 
                        c("Date","Steps"))
head(agg_date.df)
```

```
##         Date Steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

As we can observe from the "head" call, now we **ONLY** see **UNIQUE** dates, 
assuring that the above dataframe has the total number of steps taken everyday 
during the project period.


For the first part of the assignment, let's graph a histogram of the total number
of steps taken each day and also report the corresponding mean and median number
of steps.


```r
# load library and make a histogram
library(ggplot2)

ggplot(agg_date.df, aes(agg_date.df$Steps)) +
        geom_histogram(aes(y = ..density..),
                       breaks = seq(0,22000, by = 700),
                       col = "#FF2028",
                       fill = "#00FF43",
                       alpha = 0.2) +
        geom_density(col = 2) +
        labs(title = "Histogram of Daily Steps") + 
        labs(x = "Steps per Day", y = "Frequency")
```

![](rr_pa1_files/figure-html/part 2(b)-1.png)\


```r
# mean and median
mean_org <- mean(agg_date.df$Steps)
median_org <- median(agg_date.df$Steps)
print(mean_org)
```

```
## [1] 10766.19
```

```r
print(median_org)
```

```
## [1] 10765
```

The mean and the median differ by 1.19 steps, not significantly different from 
each other implying that the datapoints are fairly equally distributed.


Moving on to the next question in the assignment - **Average Daily Activity Pattern**.  

For this purpose, we would be averaging the total number of steps taken in a day
over the 5-minute intervals for all the days present in the dataset. Subsequently,
we find that **on an average**, people are most active during the **835th 5-minute interval**, 
during the project period.


```r
# averaging the number of steps over 5-minute intervals 
agg_int.df <- setNames(aggregate(org.df$steps ~ org.df$interval, 
                                 data = org.df, FUN = mean), 
                       c("Interval","Steps"))

# report the interval with the highest mean number of steps 
max_int_avg <- agg_int.df[which.max(agg_int.df$Steps),]
print(max_int_avg)
```

```
##     Interval    Steps
## 104      835 206.1698
```


Let's visualize the same by way of a time series plot.


```r
# time series plot of average steps across intervals
ggplot(agg_int.df, aes(x = Interval, y = Steps)) +
        geom_line(color = "blue") +
        labs(title = "Time Series of Daily Average Steps over Intervals") +
        labs(x = "Intervals", y = "Number of Steps")
```

![](rr_pa1_files/figure-html/part 3(a)-1.png)\


**Missing values!** Like any real dataset, the current dataset is also riddled with 
missing values.  
So, let's analyze the number of missing value cases and also the corresponding variables


```r
# number of missing values
missing_val <- sapply(org.df, function(x) sum(is.na(x)))
print(missing_val)
```

```
##    steps     date interval 
##     2304        0        0
```

Approximately, missing value cases constitute **13.11%** of the dataset, a 
fairly significant proportion, which is entirely from the "steps" variable.  

So, a simple **mean value imputation** is carried out throughout the dataset. 


```r
# copy original dataset
imp.df <- org.df

# impute missing values with the mean
imp.df$steps[is.na(imp.df$steps)] <- mean(imp.df$steps, na.rm = TRUE)
str(imp.df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


Again, let's aggregate the total number of steps over unique dates and report the 
mean and median of the same.


```r
# aggregate the imputed data set to get the total number of steps taken daily
agg_date_imp.df <- setNames(aggregate(imp.df$steps ~ imp.df$date, 
                                      data = imp.df, FUN = sum), 
                            c("Date","Steps"))

# mean and median of imputed dataset
mean_imp <- mean(agg_date_imp.df$Steps)
median_imp <- median(agg_date_imp.df$Steps)
print(mean_imp)
```

```
## [1] 10766.19
```

```r
print(median_imp)
```

```
## [1] 10766.19
```

After imputing the missing values with the mean, we find that the **mean** and the 
**median** are now **equal**. Previously, they differed by only 1.19 steps. So, 
in this case, since the original data (with missing values) was equally distributed, 
there is **none to negligible impact** on the dataset, as a result of missing value
imputation. 

Let's visualize the distribution of total number of steps taken each day after 
imputing for missing values.


```r
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
```

![](rr_pa1_files/figure-html/part 4 (d)(ii)-1.png)\


Before we wrap up, let's analyze the dataset for activity distributions from a
weekday Vs. weekend standpoint. Consequently, we would need to engineer a **new feature**, 
which I would title as "**DayType**" in this case. Basically, this would be a 
2-level factor variable, namely - **Weekday** or **Weekend**.


```r
# add DayType (weekend/weekday) feature into the imputed dataset
weekend <- c("Saturday","Sunday")
imp.df$DayType <- as.factor(ifelse(weekdays(imp.df$date) %in% weekend, "Weekend", "Weekday"))
head(imp.df)
```

```
##     steps       date interval DayType
## 1 37.3826 2012-10-01        0 Weekday
## 2 37.3826 2012-10-01        5 Weekday
## 3 37.3826 2012-10-01       10 Weekday
## 4 37.3826 2012-10-01       15 Weekday
## 5 37.3826 2012-10-01       20 Weekday
## 6 37.3826 2012-10-01       25 Weekday
```

```r
str(imp.df)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ DayType : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```


As we can see from the "str" call, there are two levels to the new factor variable - 
Weekday and Weekend.  

Okay, now let's average the number of steps taken in the imputed dataset over 
intervals for all the days.


```r
# aggregate data points over intervals
agg_int_imp.df <- setNames(aggregate(imp.df$steps ~ imp.df$interval + imp.df$DayType, 
                                     data = imp.df, FUN = mean), 
                           c("Interval","DayType", "Steps"))
head(agg_int_imp.df)
```

```
##   Interval DayType    Steps
## 1        0 Weekday 7.006569
## 2        5 Weekday 5.384347
## 3       10 Weekday 5.139902
## 4       15 Weekday 5.162124
## 5       20 Weekday 5.073235
## 6       25 Weekday 6.295458
```

```r
tail(agg_int_imp.df)
```

```
##     Interval DayType     Steps
## 571     2330 Weekend  5.735325
## 572     2335 Weekend 15.672825
## 573     2340 Weekend 10.547825
## 574     2345 Weekend  6.297825
## 575     2350 Weekend  4.672825
## 576     2355 Weekend  4.672825
```


Finally, let's visualize the mean number of steps over different intervals in a 
weekday Vs. weekend perspective to observe a pattern (if any).


```r
# time series panel plot for Interval Mean Steps - Weekday Vs. Weekend
ggplot(agg_int_imp.df, aes(x = Interval, y = Steps, color = DayType)) +
        geom_line() +
        facet_wrap(~DayType, ncol = 1, nrow = 2) +
        labs(title = "Interval Mean Steps - Weekday Vs. Weekend") + 
        labs(x = "Interval", y = "Mean Steps")
```

![](rr_pa1_files/figure-html/part 5 (b)(ii)-1.png)\


From the sharp peaks in the weekdays, we can conclude that during the weekdays
there are time periods when there is peak activity and the rest of the day is 
relatively low activity. The peak activity falls between 1pm and 3pm, presumably 
users heading out for lunch or away from work desk activities. Contrastingly, 
over the weekends, it looks like the users are consistently active throughout 
the day and there are no drastic peaks that signal peak activity times.

================================================================================================================

# Reproducible Research: Peer Assessment 1
Just testing first commit.Do we have latex math $y = \sum_{i=1}^n x^i$, Some R-code 


```r
a <- 2
b <- 3
a + b
```

```
## [1] 5
```

New test commit. Had to set up ssh to get push to work from xstudio. I hope it works now.

## Some utility functions and library needed


```r
# Function to read ziped files. Solution due to Jack Wasey at Stackoverflow
# In thread http://stackoverflow.com/questions/8986818/automate-zip-file-reading-in-r
read.zip.url <- function(url, filename = NULL, FUN = readLines, ...) {
  zipfile <- tempfile()
  download.file(url = url, destfile = zipfile, quiet = TRUE)
  zipdir <- tempfile()
  dir.create(zipdir)
  unzip(zipfile, exdir = zipdir) # files="" so extract all
  files <- list.files(zipdir)
  if (is.null(filename)) {
    if (length(files) == 1) {
      filename <- files
    } else {
      stop("multiple files in zip, but no filename specified: ", paste(files, collapse = ", "))
    }
  } else { # filename specified
    stopifnot(length(filename) ==1)
    stopifnot(filename %in% files)
  }
  file <- paste(zipdir, files[1], sep="/")
  do.call(FUN, args = c(list(file.path(zipdir, filename)), list(...)))
}
# Looks like we might use dplyr
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(zoo)
```

```
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```


## Loading and preprocessing the data

The data for this assigment is provided as a ziped .csv file. To read the data into R we could first unzip the file in a unix terminal and then read the file with the $\texttt{read.csv()}$ function. At the same time it would be nice to have a general solution to read at ziped file directly into R. A quick search turned up the following solution on Stackoverflow (http://stackoverflow.com/questions/8986818/automate-zip-file-reading-in-r) due to Jack Wasey


```r
#Set wd to local git directory
setwd('/Users/ag/dev/gitProjects/RepData_PeerAssessment1')
zipfile <- "activity.zip"
# Use utility function defined above to read in the ziped file
myData <- read.zip.url(paste("file://", zipfile, sep=""), FUN=read.csv, header=TRUE)
head(myData,n=10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

```r
#check class
class(myData)
```

```
## [1] "data.frame"
```

It looks like it might be convenient to have the data as a timeseries object too. Hence we convert the interval variable to a POSIXct object called time. This will be time since midnight. This varible is then paste toghether with the date variable and we obtain a variable dateTime that ought to give a unique point in time. However this does not seems to be the case so there is probably some problem in the dataset. We use the timeDate variable to make a zoo object, but have to exclude duplicate dates.  


```r
# generate time from interval
myData$time <- format( as.POSIXct(Sys.Date()) + myData$interval*60, format="%H:%M:%S", tz="UCT")
head(myData,n=10)
```

```
##    steps       date interval     time
## 1     NA 2012-10-01        0 00:00:00
## 2     NA 2012-10-01        5 00:05:00
## 3     NA 2012-10-01       10 00:10:00
## 4     NA 2012-10-01       15 00:15:00
## 5     NA 2012-10-01       20 00:20:00
## 6     NA 2012-10-01       25 00:25:00
## 7     NA 2012-10-01       30 00:30:00
## 8     NA 2012-10-01       35 00:35:00
## 9     NA 2012-10-01       40 00:40:00
## 10    NA 2012-10-01       45 00:45:00
```

```r
myData$dateTime <- as.POSIXct(strptime(paste(myData$date,myData$time), "%Y-%m-%d %H:%M:%S"))
head(myData,20)
```

```
##    steps       date interval     time            dateTime
## 1     NA 2012-10-01        0 00:00:00 2012-10-01 00:00:00
## 2     NA 2012-10-01        5 00:05:00 2012-10-01 00:05:00
## 3     NA 2012-10-01       10 00:10:00 2012-10-01 00:10:00
## 4     NA 2012-10-01       15 00:15:00 2012-10-01 00:15:00
## 5     NA 2012-10-01       20 00:20:00 2012-10-01 00:20:00
## 6     NA 2012-10-01       25 00:25:00 2012-10-01 00:25:00
## 7     NA 2012-10-01       30 00:30:00 2012-10-01 00:30:00
## 8     NA 2012-10-01       35 00:35:00 2012-10-01 00:35:00
## 9     NA 2012-10-01       40 00:40:00 2012-10-01 00:40:00
## 10    NA 2012-10-01       45 00:45:00 2012-10-01 00:45:00
## 11    NA 2012-10-01       50 00:50:00 2012-10-01 00:50:00
## 12    NA 2012-10-01       55 00:55:00 2012-10-01 00:55:00
## 13    NA 2012-10-01      100 01:40:00 2012-10-01 01:40:00
## 14    NA 2012-10-01      105 01:45:00 2012-10-01 01:45:00
## 15    NA 2012-10-01      110 01:50:00 2012-10-01 01:50:00
## 16    NA 2012-10-01      115 01:55:00 2012-10-01 01:55:00
## 17    NA 2012-10-01      120 02:00:00 2012-10-01 02:00:00
## 18    NA 2012-10-01      125 02:05:00 2012-10-01 02:05:00
## 19    NA 2012-10-01      130 02:10:00 2012-10-01 02:10:00
## 20    NA 2012-10-01      135 02:15:00 2012-10-01 02:15:00
```

```r
myDataZoo <- zoo(myData[! duplicated(myData$dateTime),c(1,4)])
head(myDataZoo,30)
```

```
##    steps time    
## 1  <NA>  00:00:00
## 2  <NA>  00:05:00
## 3  <NA>  00:10:00
## 4  <NA>  00:15:00
## 5  <NA>  00:20:00
## 6  <NA>  00:25:00
## 7  <NA>  00:30:00
## 8  <NA>  00:35:00
## 9  <NA>  00:40:00
## 10 <NA>  00:45:00
## 11 <NA>  00:50:00
## 12 <NA>  00:55:00
## 13 <NA>  01:40:00
## 14 <NA>  01:45:00
## 15 <NA>  01:50:00
## 16 <NA>  01:55:00
## 17 <NA>  02:00:00
## 18 <NA>  02:05:00
## 19 <NA>  02:10:00
## 20 <NA>  02:15:00
## 21 <NA>  02:20:00
## 22 <NA>  02:25:00
## 23 <NA>  02:30:00
## 24 <NA>  02:35:00
## 25 <NA>  03:20:00
## 26 <NA>  03:25:00
## 27 <NA>  03:30:00
## 28 <NA>  03:35:00
## 29 <NA>  03:40:00
## 30 <NA>  03:45:00
```


## What is mean total number of steps taken per day?

Use dplyr to group the data


```r
days <- group_by(myData, as.character(myData$date))
stepsDay <- summarise(days,test=n(),sum(steps))
stepsDay
```

```
## Source: local data frame [61 x 3]
## 
##    as.character(myData$date) test sum(steps)
## 1                 2012-10-01  288         NA
## 2                 2012-10-02  288        126
## 3                 2012-10-03  288      11352
## 4                 2012-10-04  288      12116
## 5                 2012-10-05  288      13294
## 6                 2012-10-06  288      15420
## 7                 2012-10-07  288      11015
## 8                 2012-10-08  288         NA
## 9                 2012-10-09  288      12811
## 10                2012-10-10  288       9900
## 11                2012-10-11  288      10304
## 12                2012-10-12  288      17382
## 13                2012-10-13  288      12426
## 14                2012-10-14  288      15098
## 15                2012-10-15  288      10139
## 16                2012-10-16  288      15084
## 17                2012-10-17  288      13452
## 18                2012-10-18  288      10056
## 19                2012-10-19  288      11829
## 20                2012-10-20  288      10395
## 21                2012-10-21  288       8821
## 22                2012-10-22  288      13460
## 23                2012-10-23  288       8918
## 24                2012-10-24  288       8355
## 25                2012-10-25  288       2492
## 26                2012-10-26  288       6778
## 27                2012-10-27  288      10119
## 28                2012-10-28  288      11458
## 29                2012-10-29  288       5018
## 30                2012-10-30  288       9819
## 31                2012-10-31  288      15414
## 32                2012-11-01  288         NA
## 33                2012-11-02  288      10600
## 34                2012-11-03  288      10571
## 35                2012-11-04  288         NA
## 36                2012-11-05  288      10439
## 37                2012-11-06  288       8334
## 38                2012-11-07  288      12883
## 39                2012-11-08  288       3219
## 40                2012-11-09  288         NA
## 41                2012-11-10  288         NA
## 42                2012-11-11  288      12608
## 43                2012-11-12  288      10765
## 44                2012-11-13  288       7336
## 45                2012-11-14  288         NA
## 46                2012-11-15  288         41
## 47                2012-11-16  288       5441
## 48                2012-11-17  288      14339
## 49                2012-11-18  288      15110
## 50                2012-11-19  288       8841
## 51                2012-11-20  288       4472
## 52                2012-11-21  288      12787
## 53                2012-11-22  288      20427
## 54                2012-11-23  288      21194
## 55                2012-11-24  288      14478
## 56                2012-11-25  288      11834
## 57                2012-11-26  288      11162
## 58                2012-11-27  288      13646
## 59                2012-11-28  288      10183
## 60                2012-11-29  288       7047
## 61                2012-11-30  288         NA
```

```r
head(stepsDay[,3])
```

```
## [1]    NA   126 11352 12116 13294 15420
```

```r
hist(stepsDay[,3],breaks=15)
```

![plot of chunk unnamed-chunk-5](PA1_template_files/figure-html/unnamed-chunk-5.png) 

```r
#mean number of steps per day
mean(stepsDay[,3], na.rm=TRUE)
```

```
## [1] 10766
```

```r
#median number of steps per day
median(stepsDay[,3],na.rm=TRUE)
```

```
## [1] 10765
```



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?

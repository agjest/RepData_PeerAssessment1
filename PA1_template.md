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

Use dplyr to group by interval


```r
myDataNONA <- myData[! is.na(myData$steps),]
daysInterv <- group_by(myDataNONA, factor(myDataNONA$interval))
stepsInterval <- summarise(daysInterv,test=n(), sum(steps))
head(stepsInterval)
```

```
## Source: local data frame [6 x 3]
## 
##   factor(myDataNONA$interval) test sum(steps)
## 1                           0   53         91
## 2                           5   53         18
## 3                          10   53          7
## 4                          15   53          8
## 5                          20   53          4
## 6                          25   53        111
```

```r
as.numeric(as.character(stepsInterval[,1]))
```

```
##   [1]    0    5   10   15   20   25   30   35   40   45   50   55  100  105
##  [15]  110  115  120  125  130  135  140  145  150  155  200  205  210  215
##  [29]  220  225  230  235  240  245  250  255  300  305  310  315  320  325
##  [43]  330  335  340  345  350  355  400  405  410  415  420  425  430  435
##  [57]  440  445  450  455  500  505  510  515  520  525  530  535  540  545
##  [71]  550  555  600  605  610  615  620  625  630  635  640  645  650  655
##  [85]  700  705  710  715  720  725  730  735  740  745  750  755  800  805
##  [99]  810  815  820  825  830  835  840  845  850  855  900  905  910  915
## [113]  920  925  930  935  940  945  950  955 1000 1005 1010 1015 1020 1025
## [127] 1030 1035 1040 1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135
## [141] 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245
## [155] 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 1345 1350 1355
## [169] 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 1500 1505
## [183] 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610 1615
## [197] 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725
## [211] 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835
## [225] 1840 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945
## [239] 1950 1955 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055
## [253] 2100 2105 2110 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205
## [267] 2210 2215 2220 2225 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315
## [281] 2320 2325 2330 2335 2340 2345 2350 2355
```

```r
plot(as.numeric(as.character(stepsInterval[,1])),stepsInterval[,3], type = "l")
```

![plot of chunk unnamed-chunk-6](PA1_template_files/figure-html/unnamed-chunk-6.png) 

```r
paste(as.numeric(as.character(stepsInterval[,1])), as.numeric(as.character(stepsInterval[,3])))
```

```
##   [1] "0 91"      "5 18"      "10 7"      "15 8"      "20 4"     
##   [6] "25 111"    "30 28"     "35 46"     "40 0"      "45 78"    
##  [11] "50 16"     "55 7"      "100 17"    "105 36"    "110 8"    
##  [16] "115 18"    "120 0"     "125 59"    "130 97"    "135 9"    
##  [21] "140 9"     "145 20"    "150 14"    "155 0"     "200 0"    
##  [26] "205 0"     "210 60"    "215 0"     "220 0"     "225 7"    
##  [31] "230 0"     "235 12"    "240 0"     "245 0"     "250 82"   
##  [36] "255 50"    "300 0"     "305 0"     "310 0"     "315 0"    
##  [41] "320 11"    "325 33"    "330 86"    "335 31"    "340 26"   
##  [46] "345 4"     "350 0"     "355 0"     "400 63"    "405 50"   
##  [51] "410 136"   "415 0"     "420 18"    "425 19"    "430 218"  
##  [56] "435 35"    "440 185"   "445 44"    "450 165"   "455 59"   
##  [61] "500 0"     "505 83"    "510 159"   "515 119"   "520 176"  
##  [66] "525 157"   "530 111"   "535 321"   "540 849"   "545 972"  
##  [71] "550 2091"  "555 2358"  "600 1669"  "605 2611"  "610 2850" 
##  [76] "615 3363"  "620 2648"  "625 2495"  "630 2764"  "635 2085" 
##  [81] "640 2333"  "645 2341"  "650 1980"  "655 2599"  "700 2322" 
##  [86] "705 2352"  "710 2677"  "715 2889"  "720 2646"  "725 2702" 
##  [91] "730 2951"  "735 2349"  "740 2770"  "745 3686"  "750 3066" 
##  [96] "755 2976"  "800 3889"  "805 3615"  "810 6860"  "815 8349" 
## [101] "820 9071"  "825 8236"  "830 9397"  "835 10927" "840 10384"
## [106] "845 9517"  "850 9720"  "855 8852"  "900 7603"  "905 6574" 
## [111] "910 5783"  "915 5730"  "920 5497"  "925 5086"  "930 3509" 
## [116] "935 2397"  "940 1314"  "945 2054"  "950 1854"  "955 1116" 
## [121] "1000 2150" "1005 1430" "1010 2248" "1015 2791" "1020 2063"
## [126] "1025 2692" "1030 2347" "1035 1983" "1040 1839" "1045 1502"
## [131] "1050 1330" "1055 1693" "1100 1662" "1105 1573" "1110 1130"
## [136] "1115 1354" "1120 1504" "1125 1403" "1130 1772" "1135 2649"
## [141] "1140 2228" "1145 2364" "1150 2440" "1155 3137" "1200 3385"
## [146] "1205 4648" "1210 5027" "1215 4917" "1220 3360" "1225 2659"
## [151] "1230 2887" "1235 1718" "1240 1406" "1245 2000" "1250 2388"
## [156] "1255 3566" "1300 2244" "1305 2114" "1310 2293" "1315 2172"
## [161] "1320 2451" "1325 2991" "1330 2266" "1335 1332" "1340 2118"
## [166] "1345 2838" "1350 2508" "1355 3223" "1400 2955" "1405 2754"
## [171] "1410 2310" "1415 2581" "1420 1880" "1425 1990" "1430 2218"
## [176] "1435 1458" "1440 907"  "1445 1382" "1450 2312" "1455 2320"
## [181] "1500 1591" "1505 1912" "1510 1881" "1515 2059" "1520 2436"
## [186] "1525 2531" "1530 2551" "1535 3462" "1540 4394" "1545 5229"
## [191] "1550 5412" "1555 4450" "1600 3293" "1605 3399" "1610 3951"
## [196] "1615 3348" "1620 3016" "1625 3168" "1630 2325" "1635 2044"
## [201] "1640 2367" "1645 2409" "1650 2449" "1655 2315" "1700 2471"
## [206] "1705 2984" "1710 2688" "1715 3245" "1720 3854" "1725 4184"
## [211] "1730 3654" "1735 3162" "1740 3980" "1745 2995" "1750 1843"
## [216] "1755 1985" "1800 2156" "1805 3075" "1810 3959" "1815 4522"
## [221] "1820 3141" "1825 3592" "1830 4118" "1835 3935" "1840 4523"
## [226] "1845 5271" "1850 4589" "1855 4537" "1900 4498" "1905 4125"
## [231] "1910 3076" "1915 2828" "1920 1925" "1925 1098" "1930 1452"
## [236] "1935 2121" "1940 1601" "1945 1354" "1950 2420" "1955 1777"
## [241] "2000 1040" "2005 1008" "2010 1025" "2015 1767" "2020 1421"
## [246] "2025 1122" "2030 1447" "2035 1131" "2040 1036" "2045 1130"
## [251] "2050 1712" "2055 1068" "2100 845"  "2105 913"  "2110 1243"
## [256] "2115 1020" "2120 660"  "2125 425"  "2130 777"  "2135 864" 
## [261] "2140 460"  "2145 413"  "2150 431"  "2155 139"  "2200 77"  
## [266] "2205 195"  "2210 255"  "2215 451"  "2220 375"  "2225 461" 
## [271] "2230 517"  "2235 117"  "2240 17"   "2245 6"    "2250 85"  
## [276] "2255 244"  "2300 175"  "2305 151"  "2310 0"    "2315 44"  
## [281] "2320 51"   "2325 84"   "2330 138"  "2335 249"  "2340 175" 
## [286] "2345 34"   "2350 12"   "2355 57"
```

```r
max(stepsInterval[,3])
```

```
## [1] 10927
```

```r
stepsInterval[stepsInterval$"sum(steps)"=="10927",]
```

```
## Source: local data frame [1 x 3]
## 
##     factor(myDataNONA$interval) test sum(steps)
## 104                         835   53      10927
```



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?

---
title: "CaseStudy2"
author: "Thejas Prasad , Joseph Schueder"
date: "11/28/2017"
output: 
  html_document:
    keep_md: true
---



## Case Study 2 Procrastination


```r
# Initialize Libraries
library(data.table)
library("rvest") # Great for grabbing and parsing HTML
```

```
## Loading required package: xml2
```

```r
library("dplyr") # Easy way for transformations of data.frames for summarization
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library("tidyr") # Nice way to arrange data
library("ggplot2") # Excellent for visuals

# Importing the dataset

#dt -> Data Table
procrasti_dt = data.table(read.table('Procrastination.csv',sep = ",",header = TRUE, as.is = TRUE, check.names=FALSE, quote = "\""))
str(procrasti_dt)
```

```
## Classes 'data.table' and 'data.frame':	4264 obs. of  61 variables:
##  $ Age                                                                                                                    : num  67.5 45 19 37.5 28 23 67.5 37.5 24 45 ...
##  $ Gender                                                                                                                 : chr  "Male" "Male" "Female" "Male" ...
##  $ Kids                                                                                                                   : chr  "Yes Kids" "Yes Kids" "No Kids" "Yes Kids" ...
##  $ Edu                                                                                                                    : chr  "ma" "deg" "dip" "ma" ...
##  $ Work Status                                                                                                            : chr  "retired" "part-time" "student" "full-time" ...
##  $ Annual Income                                                                                                          : int  25000 35000 NA 45000 35000 15000 NA 10000 250000 87500 ...
##  $ Current Occupation                                                                                                     : chr  "" "" "" "" ...
##  $ How long have you held this position?: Years                                                                           : num  9.0 1.5e-19 0.0 1.4e+01 1.0 ...
##  $ How long have you held this position?: Months                                                                          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Community size                                                                                                         : chr  "Large-City" "Village" "Large Town" "Large Town" ...
##  $ Country of residence                                                                                                   : chr  "El Salvador" "Bolivia" "Cyprus" "Czech Republic" ...
##  $ Marital Status                                                                                                         : chr  "Divorced" "Married" "Single" "Married" ...
##  $ Number of sons                                                                                                         : chr  "0" "Male" "0" "0" ...
##  $ Number of daughters                                                                                                    : int  5 1 0 1 0 0 0 0 0 0 ...
##  $ (DP 1) I waste a lot of time on trivial matters before getting to the final decisions                                  : int  3 3 5 3 3 3 3 4 2 5 ...
##  $ (DP 2) Even after I make a decision I delay acting upon it                                                             : int  1 4 5 3 3 4 4 3 2 5 ...
##  $ (DP 3) I don’t make decisions unless I really have to                                                                  : int  1 3 2 3 2 3 3 4 4 5 ...
##  $ (DP 4) I delay making decisions until it’s too late                                                                    : int  1 3 3 3 1 2 2 4 4 5 ...
##  $ (DP 5) I put off making decisions until it’s too late                                                                  : int  1 3 3 3 1 2 2 3 4 5 ...
##  $ (AIP 1) I pay my bills on time                                                                                         : int  1 3 5 2 1 2 3 4 3 3 ...
##  $ (AIP 2)I am prompt and on time for most appointments.                                                                  : int  1 1 4 1 1 5 1 4 3 3 ...
##  $ (AIP 3)I lay out my clothes the night before I have an important appointment, so I won’t be late                       : int  1 4 4 4 3 5 1 4 3 5 ...
##  $ (AIP 4) I find myself running later than I would like to be                                                            : int  1 3 5 3 3 5 2 4 4 3 ...
##  $ (AIP 5) I don’t get things done on time                                                                                : int  1 3 5 5 2 5 3 4 4 5 ...
##  $ (AIP 6) If someone were teaching a course on how to get things done on time, I would attend                            : int  1 4 5 3 2 3 3 2 2 1 ...
##  $ (AIP 7) My friends and family think I wait until the last minute.                                                      : int  1 3 5 4 2 5 1 5 2 5 ...
##  $ (AIP 8) I get important things done with time to spare                                                                 : int  1 3 4 5 2 4 4 2 4 5 ...
##  $ (AIP 9) I am not very good at meeting deadlines                                                                        : int  5 3 5 4 1 4 5 4 2 5 ...
##  $ (AIP 10) I find myself running out of time.                                                                            : int  1 3 5 5 1 5 5 3 2 5 ...
##  $ (AIP 11) I schedule doctor’s appointments when I am supposed to without delay                                          : int  1 4 4 4 2 3 3 2 4 4 ...
##  $ (AIP 12) I am more punctual than most people I know                                                                    : int  1 2 3 3 1 5 2 4 4 4 ...
##  $ (AIP 13) I do routine maintenance (e.g., changing the car oil) on things I own as often as I should                    : int  1 2 5 4 2 4 3 3 4 4 ...
##  $ (AIP 14)When I have to be somewhere at a certain time my friends expect me to run a bit late                           : int  1 2 4 2 1 5 1 4 3 4 ...
##  $ (AIP 15)Putting things off till the last minute has cost me money in the past                                          : int  3 4 3 1 2 5 4 5 3 5 ...
##  $ (GP 1)I often find myself performing tasks that I had intended to do days before                                       : int  1 4 5 4 4 5 4 5 3 5 ...
##  $ (GP2) I often miss concerts, sporting events, or the like because I don’t get around to buying tickets on time         : int  1 2 2 1 1 5 1 1 4 3 ...
##  $ (GP 3) When planning a party, I make the necessary arrangements well in advance                                        : int  1 2 2 3 2 2 1 3 3 3 ...
##  $ (GP 4) When it is time to get up in the morning, I most often get right out of bed                                     : int  1 2 4 3 4 5 1 4 4 1 ...
##  $ (GP 5) A letter may sit for days after I write it before mailing it possible                                           : int  1 2 3 2 5 4 1 3 2 5 ...
##  $ (GP 6) I generally return phone calls promptly                                                                         : int  1 2 1 3 2 4 2 3 2 5 ...
##  $ (GP 7) Even jobs that require little else except sitting down and doing them, I find that they seldom get done for days: int  1 4 3 4 4 5 3 4 4 5 ...
##  $ (GP 8) I usually make decisions as soon as possible                                                                    : int  1 2 2 5 2 4 2 3 2 4 ...
##  $ (GP 9) I generally delay before starting on work I have to do                                                          : int  1 4 5 4 4 4 4 4 4 5 ...
##  $ (GP 10) When traveling, I usually have to rush in preparing to arrive at the airport or station at the appropriate time: int  1 2 4 1 1 3 1 4 1 3 ...
##  $ (GP 11) When preparing to go out, I am seldom caught having to do something at the last minute                         : int  5 3 5 3 2 4 4 3 5 5 ...
##  $ (GP 12) In preparation for some deadlines, I often waste time by doing other things                                    : int  1 4 5 4 3 4 2 5 2 5 ...
##  $ (GP 13) If a bill for a small amount comes, I pay it right away                                                        : int  1 2 3 3 2 3 3 2 3 4 ...
##  $ (GP 14) I usually return a “RSVP” request very shortly after receiving it                                              : int  1 2 4 3 4 4 2 2 2 4 ...
##  $ (GP 15) I often have a task finished sooner than necessary                                                             : int  1 3 5 4 3 4 4 4 1 4 ...
##  $ (GP 16) I always seem to end up shopping for birthday gifts at the last minute                                         : int  1 4 2 4 2 4 3 4 5 5 ...
##  $ (GP 17) I usually buy even an essential item at the last minute                                                        : int  1 3 3 3 3 4 1 3 5 3 ...
##  $ (GP 18) I usually accomplish all the things I plan to do in a day                                                      : int  5 3 5 4 2 4 4 4 1 5 ...
##  $ (GP 19) I am continually saying “I’ll do it tomorrow”                                                                  : int  1 4 5 5 3 4 4 4 1 5 ...
##  $ (GP 20) I usually take care of all the tasks I have to do before I settle down and relax for the evening               : int  5 4 4 1 4 4 2 4 3 5 ...
##  $ (SWLS 1) In most ways my life is close to my ideal                                                                     : int  5 3 2 2 4 3 3 3 4 1 ...
##  $ (SWLS 2)The conditions of my life are excellent                                                                        : int  5 4 2 4 4 2 4 3 4 4 ...
##  $ (SWLS 3) I am satisfied with my life.                                                                                  : int  5 4 2 2 4 4 3 3 5 2 ...
##  $ (SWLS 4) So far I have gotten the important things I want in life                                                      : int  5 4 3 2 3 4 3 2 4 4 ...
##  $ (SWLS 5) If I could live my life over, I would change almost nothing                                                   : int  5 3 4 2 4 3 2 3 4 1 ...
##  $ Do you consider yourself a procrastinator?                                                                             : chr  "no" "yes" "yes" "yes" ...
##  $ Do others consider you a procrastinator?                                                                               : chr  "no" "yes" "yes" "yes" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
#?read.csv
# 2.a No Of Rows & No Of Columns - reduce to 12 characters

nrow(procrasti_dt)
```

```
## [1] 4264
```

```r
ncol(procrasti_dt)
```

```
## [1] 61
```

```r
# 2.b Fix Column Names

setnames(procrasti_dt, "Age", "Age")
setnames(procrasti_dt, "Gender", "Gender")
setnames(procrasti_dt, "Kids", "Kids")
setnames(procrasti_dt, "Edu", "Education")
setnames(procrasti_dt, "Work Status", "WorkStatus")
setnames(procrasti_dt, "Annual Income", "AnnualIncome")
setnames(procrasti_dt, "Current Occupation", "CurrentOccup")
setnames(procrasti_dt, "How long have you held this position?: Years", "CurrPosYrs")
setnames(procrasti_dt, "How long have you held this position?: Months", "CurrPosMon")
setnames(procrasti_dt, "Community size", "CommuSize")
setnames(procrasti_dt, "Country of residence", "CountryOfRes")
setnames(procrasti_dt, "Marital Status", "MaritalStat")
setnames(procrasti_dt, "Number of sons", "NoOfSons")
setnames(procrasti_dt, "Number of daughters", "NoOfDaughtrs")
setnames(procrasti_dt, "(DP 1) I waste a lot of time on trivial matters before getting to the final decisions", "DP1WaTrivial")
setnames(procrasti_dt, "(DP 2) Even after I make a decision I delay acting upon it", "DP2DelayAct")
setnames(procrasti_dt, "(DP 3) I don’t make decisions unless I really have to", "DP3MakNoDeci")
setnames(procrasti_dt, "(DP 4) I delay making decisions until it’s too late", "DP4DelyDeci")
setnames(procrasti_dt, "(DP 5) I put off making decisions until it’s too late", "DP5PutOfDeci")
setnames(procrasti_dt, "(AIP 1) I pay my bills on time", "AIP1PayBilOT")
setnames(procrasti_dt, "(AIP 2)I am prompt and on time for most appointments.", "AIP2OnTimApp")
setnames(procrasti_dt, "(AIP 3)I lay out my clothes the night before I have an important appointment, so I won’t be late", "AIP3LayOTClo")
setnames(procrasti_dt, "(AIP 4) I find myself running later than I would like to be", "AIP4RunLater")
setnames(procrasti_dt, "(AIP 5) I don’t get things done on time", "AIP5DonOnTim")
setnames(procrasti_dt, "(AIP 6) If someone were teaching a course on how to get things done on time, I would attend", "AIP6AttCours")
setnames(procrasti_dt, "(AIP 7) My friends and family think I wait until the last minute.", "AIP7FFWaitLM")
setnames(procrasti_dt, "(AIP 8) I get important things done with time to spare", "AIP8GetDonTS")
setnames(procrasti_dt, "(AIP 9) I am not very good at meeting deadlines", "AIP9NtGdMeDe")
setnames(procrasti_dt, "(AIP 10) I find myself running out of time.", "AIP10RnOtTim")
setnames(procrasti_dt, "(AIP 11) I schedule doctor’s appointments when I am supposed to without delay", "AIP11ScDrApp")
setnames(procrasti_dt, "(AIP 12) I am more punctual than most people I know", "AIP12IamPunc")
setnames(procrasti_dt, "(AIP 13) I do routine maintenance (e.g., changing the car oil) on things I own as often as I should", "AIP13DoRoMai")
setnames(procrasti_dt, "(AIP 14)When I have to be somewhere at a certain time my friends expect me to run a bit late", "AIP14FExpLat")
setnames(procrasti_dt, "(AIP 15)Putting things off till the last minute has cost me money in the past", "AIP15PtLsMin")
setnames(procrasti_dt, "(GP 1)I often find myself performing tasks that I had intended to do days before", "GP1PrTskLate")
setnames(procrasti_dt, "(GP2) I often miss concerts, sporting events, or the like because I don’t get around to buying tickets on time", "GP2MissEvnts")
setnames(procrasti_dt, "(GP 3) When planning a party, I make the necessary arrangements well in advance", "GP3PlPrtyAdv")
setnames(procrasti_dt, "(GP 4) When it is time to get up in the morning, I most often get right out of bed", "GP4MgGetUpOT")
setnames(procrasti_dt, "(GP 5) A letter may sit for days after I write it before mailing it possible", "GP5MailLtrLt")
setnames(procrasti_dt, "(GP 6) I generally return phone calls promptly", "GP6RtPhPromt")
setnames(procrasti_dt, "(GP 7) Even jobs that require little else except sitting down and doing them, I find that they seldom get done for days", "GP7DlyEsyJob")
setnames(procrasti_dt, "(GP 8) I usually make decisions as soon as possible", "GP8MkDesAsap")
setnames(procrasti_dt, "(GP 9) I generally delay before starting on work I have to do", "GP9DlyStrWrk")
setnames(procrasti_dt, "(GP 10) When traveling, I usually have to rush in preparing to arrive at the airport or station at the appropriate time", "GP10RshFoTvl")
setnames(procrasti_dt, "(GP 11) When preparing to go out, I am seldom caught having to do something at the last minute", "GP11PrWelGoO")
setnames(procrasti_dt, "(GP 12) In preparation for some deadlines, I often waste time by doing other things", "GP12WaTimOth")
setnames(procrasti_dt, "(GP 13) If a bill for a small amount comes, I pay it right away", "GP13PySmBilI")
setnames(procrasti_dt, "(GP 14) I usually return a “RSVP” request very shortly after receiving it", "GP14RtRSVPIm")
setnames(procrasti_dt, "(GP 15) I often have a task finished sooner than necessary", "GP15FnTskSnr")
setnames(procrasti_dt, "(GP 16) I always seem to end up shopping for birthday gifts at the last minute", "GP16ShBDGfLM")
setnames(procrasti_dt, "(GP 17) I usually buy even an essential item at the last minute", "GP17ShEsItLM")
setnames(procrasti_dt, "(GP 18) I usually accomplish all the things I plan to do in a day", "GP18AccInDay")
setnames(procrasti_dt, "(GP 19) I am continually saying “I’ll do it tomorrow”", "GP19IDoItTom")
setnames(procrasti_dt, "(GP 20) I usually take care of all the tasks I have to do before I settle down and relax for the evening", "GP20IFnAlTsk")
setnames(procrasti_dt, "(SWLS 1) In most ways my life is close to my ideal", "SWLS1LifCIdl")
setnames(procrasti_dt, "(SWLS 2)The conditions of my life are excellent", "SWLS2LiCnExl")
setnames(procrasti_dt, "(SWLS 3) I am satisfied with my life.", "SWLS3SatisLf")
setnames(procrasti_dt, "(SWLS 4) So far I have gotten the important things I want in life", "SWLS4GtImpTh")
setnames(procrasti_dt, "(SWLS 5) If I could live my life over, I would change almost nothing", "SWLS5SamLfAg")
setnames(procrasti_dt, "Do you consider yourself a procrastinator?", "SlfConProCra")
setnames(procrasti_dt, "Do others consider you a procrastinator?", "OtrConProCra")

str(procrasti_dt)
```

```
## Classes 'data.table' and 'data.frame':	4264 obs. of  61 variables:
##  $ Age         : num  67.5 45 19 37.5 28 23 67.5 37.5 24 45 ...
##  $ Gender      : chr  "Male" "Male" "Female" "Male" ...
##  $ Kids        : chr  "Yes Kids" "Yes Kids" "No Kids" "Yes Kids" ...
##  $ Education   : chr  "ma" "deg" "dip" "ma" ...
##  $ WorkStatus  : chr  "retired" "part-time" "student" "full-time" ...
##  $ AnnualIncome: int  25000 35000 NA 45000 35000 15000 NA 10000 250000 87500 ...
##  $ CurrentOccup: chr  "" "" "" "" ...
##  $ CurrPosYrs  : num  9.0 1.5e-19 0.0 1.4e+01 1.0 ...
##  $ CurrPosMon  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CommuSize   : chr  "Large-City" "Village" "Large Town" "Large Town" ...
##  $ CountryOfRes: chr  "El Salvador" "Bolivia" "Cyprus" "Czech Republic" ...
##  $ MaritalStat : chr  "Divorced" "Married" "Single" "Married" ...
##  $ NoOfSons    : chr  "0" "Male" "0" "0" ...
##  $ NoOfDaughtrs: int  5 1 0 1 0 0 0 0 0 0 ...
##  $ DP1WaTrivial: int  3 3 5 3 3 3 3 4 2 5 ...
##  $ DP2DelayAct : int  1 4 5 3 3 4 4 3 2 5 ...
##  $ DP3MakNoDeci: int  1 3 2 3 2 3 3 4 4 5 ...
##  $ DP4DelyDeci : int  1 3 3 3 1 2 2 4 4 5 ...
##  $ DP5PutOfDeci: int  1 3 3 3 1 2 2 3 4 5 ...
##  $ AIP1PayBilOT: int  1 3 5 2 1 2 3 4 3 3 ...
##  $ AIP2OnTimApp: int  1 1 4 1 1 5 1 4 3 3 ...
##  $ AIP3LayOTClo: int  1 4 4 4 3 5 1 4 3 5 ...
##  $ AIP4RunLater: int  1 3 5 3 3 5 2 4 4 3 ...
##  $ AIP5DonOnTim: int  1 3 5 5 2 5 3 4 4 5 ...
##  $ AIP6AttCours: int  1 4 5 3 2 3 3 2 2 1 ...
##  $ AIP7FFWaitLM: int  1 3 5 4 2 5 1 5 2 5 ...
##  $ AIP8GetDonTS: int  1 3 4 5 2 4 4 2 4 5 ...
##  $ AIP9NtGdMeDe: int  5 3 5 4 1 4 5 4 2 5 ...
##  $ AIP10RnOtTim: int  1 3 5 5 1 5 5 3 2 5 ...
##  $ AIP11ScDrApp: int  1 4 4 4 2 3 3 2 4 4 ...
##  $ AIP12IamPunc: int  1 2 3 3 1 5 2 4 4 4 ...
##  $ AIP13DoRoMai: int  1 2 5 4 2 4 3 3 4 4 ...
##  $ AIP14FExpLat: int  1 2 4 2 1 5 1 4 3 4 ...
##  $ AIP15PtLsMin: int  3 4 3 1 2 5 4 5 3 5 ...
##  $ GP1PrTskLate: int  1 4 5 4 4 5 4 5 3 5 ...
##  $ GP2MissEvnts: int  1 2 2 1 1 5 1 1 4 3 ...
##  $ GP3PlPrtyAdv: int  1 2 2 3 2 2 1 3 3 3 ...
##  $ GP4MgGetUpOT: int  1 2 4 3 4 5 1 4 4 1 ...
##  $ GP5MailLtrLt: int  1 2 3 2 5 4 1 3 2 5 ...
##  $ GP6RtPhPromt: int  1 2 1 3 2 4 2 3 2 5 ...
##  $ GP7DlyEsyJob: int  1 4 3 4 4 5 3 4 4 5 ...
##  $ GP8MkDesAsap: int  1 2 2 5 2 4 2 3 2 4 ...
##  $ GP9DlyStrWrk: int  1 4 5 4 4 4 4 4 4 5 ...
##  $ GP10RshFoTvl: int  1 2 4 1 1 3 1 4 1 3 ...
##  $ GP11PrWelGoO: int  5 3 5 3 2 4 4 3 5 5 ...
##  $ GP12WaTimOth: int  1 4 5 4 3 4 2 5 2 5 ...
##  $ GP13PySmBilI: int  1 2 3 3 2 3 3 2 3 4 ...
##  $ GP14RtRSVPIm: int  1 2 4 3 4 4 2 2 2 4 ...
##  $ GP15FnTskSnr: int  1 3 5 4 3 4 4 4 1 4 ...
##  $ GP16ShBDGfLM: int  1 4 2 4 2 4 3 4 5 5 ...
##  $ GP17ShEsItLM: int  1 3 3 3 3 4 1 3 5 3 ...
##  $ GP18AccInDay: int  5 3 5 4 2 4 4 4 1 5 ...
##  $ GP19IDoItTom: int  1 4 5 5 3 4 4 4 1 5 ...
##  $ GP20IFnAlTsk: int  5 4 4 1 4 4 2 4 3 5 ...
##  $ SWLS1LifCIdl: int  5 3 2 2 4 3 3 3 4 1 ...
##  $ SWLS2LiCnExl: int  5 4 2 4 4 2 4 3 4 4 ...
##  $ SWLS3SatisLf: int  5 4 2 2 4 4 3 3 5 2 ...
##  $ SWLS4GtImpTh: int  5 4 3 2 3 4 3 2 4 4 ...
##  $ SWLS5SamLfAg: int  5 3 4 2 4 3 2 3 4 1 ...
##  $ SlfConProCra: chr  "no" "yes" "yes" "yes" ...
##  $ OtrConProCra: chr  "no" "yes" "yes" "yes" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
#procrasti_dt <- data.table(procrasti_dt)
#---Above Names from Excel

# 2.c 1 Fix values for Column CurrPosYrs - round to nearest interger and update 999 to NA

procrasti_dt[ , CurrPosYrs := .(round(CurrPosYrs,1)), ]

procrasti_dt[ CurrPosYrs == 999, CurrPosYrs := NA, ]

#To count the no of rows with age = 45 and group by Eduction
#procrasti_dt[ Age == 45, .N, by = Education]
#To count the no of rows  group by Eduction
#procrasti_dt[ , .N, by = Education]


#Test the results below
head(round(procrasti_dt$CurrPosYrs,1))
```

```
## [1]  9  0  0 14  1  1
```

```r
head(procrasti_dt$CurrPosYrs)
```

```
## [1]  9  0  0 14  1  1
```

```r
head(max(procrasti_dt$CurrPosYrs) > 70, na.rm = TRUE)
```

```
## [1] NA
```

```r
max(procrasti_dt[, CurrPosYrs],na.rm = TRUE)
```

```
## [1] 30
```

```r
# 2.c 2 No Of Sons fix the values and chnge the data type to integers

procrasti_dt[ NoOfSons == "Male", NoOfSons := "1" ,  ]
procrasti_dt[ NoOfSons == "Female", NoOfSons := "2" ,  ]

procrasti_dt[ , NoOfSons := .(as.integer(NoOfSons)),  ]

#Test the results below
head(procrasti_dt$NoOfSons)
```

```
## [1] 0 1 0 0 0 0
```

```r
#str(procrasti_dt)

# 2.c 3 Treat 0 as NA in the CountryOfRes

# Get count 1st
procrasti_dt[ CountryOfRes == "0" , .N, ]
```

```
## [1] 233
```

```r
#procrasti_dt[ CountryOfRes == "NA", .N, ]
procrasti_dt[ CountryOfRes == "", .N, ]
```

```
## [1] 3
```

```r
# Change to NA
procrasti_dt[ CountryOfRes == "0", CountryOfRes := NA, ]
procrasti_dt[ CountryOfRes == "", CountryOfRes := NA, ]

# Test Results Get count --should be 0
procrasti_dt[ CountryOfRes == "0", .N, ]
```

```
## [1] 0
```

```r
# 2.c 4 Treat 0 and 'please specify'as NA in the Current Occupation column, then categorize the jobs

# Get count 1st
procrasti_dt[ CurrentOccup == "0", .N, ]
```

```
## [1] 488
```

```r
procrasti_dt[ CurrentOccup == "please specify", .N, ]
```

```
## [1] 217
```

```r
procrasti_dt[ CurrentOccup == "", .N, ]
```

```
## [1] 2162
```

```r
# Change to NA
procrasti_dt[ CurrentOccup == "0", CurrentOccup := NA, ]
procrasti_dt[ CurrentOccup == "please specify", CurrentOccup := NA, ]
procrasti_dt[ CurrentOccup == "", CurrentOccup := NA, ]

# Test Results Get count --should be 0
procrasti_dt[ CurrentOccup == "0", .N, ]
```

```
## [1] 0
```

```r
procrasti_dt[ CurrentOccup == "please specify", .N, ]
```

```
## [1] 0
```

```r
procrasti_dt[ CurrentOccup == "", .N, ]
```

```
## [1] 0
```

```r
# Categorize the jobs

# Get count 1st
procrasti_dt[ CurrentOccup %in%  c(" teacher",
                                   "EFL Teacher/ Professional Researcher",
                                   "Dance teacher",
                                   "ESL Teacher/Biologist",
                                   "Freelance ESL Teacher",
                                   "special education teacher",
                                   "yoga teacher",
                                   "Teacher assistant",
                                   "musician/student/teacher",
                                   "Teacher and Full Time Doctoral Student",
                                   "teacher's assistant/afterschool leader",
                                   "Asst. Pre-school Teacher",
                                   "Student/Teacher",
                                   "Theater artist/ Teacher",
                                   "Early child hood teacher",
                                   "teacher / Administrator"), .N,]
```

```
## [1] 94
```

```r
#head(procrasti_dt$CurrentOccup == " teacher" , 113)
#head(procrasti_dt$CurrentOccup  , 113)

# Change the Job Name to Teacher

procrasti_dt[ CurrentOccup %in%  c(" teacher",
                                   "EFL Teacher/ Professional Researcher",
                                   "Dance teacher",
                                   "ESL Teacher/Biologist",
                                   "Freelance ESL Teacher",
                                   "special education teacher",
                                   "yoga teacher",
                                   "Teacher assistant",
                                   "musician/student/teacher",
                                   "Teacher and Full Time Doctoral Student",
                                   "teacher's assistant/afterschool leader",
                                   "Asst. Pre-school Teacher",
                                   "Student/Teacher",
                                   "Theater artist/ Teacher",
                                   "Early child hood teacher",
                                   "teacher / Administrator"), CurrentOccup := "Teacher",]




#Test Resluts
procrasti_dt[ CurrentOccup %in%  c(" teacher",
"EFL Teacher/ Professional Researcher",
"Dance teacher",
"ESL Teacher/Biologist",
"Freelance ESL Teacher",
"special education teacher",
"yoga teacher",
"Teacher assistant",
"musician/student/teacher",
"Teacher and Full Time Doctoral Student",
"teacher's assistant/afterschool leader",
"Asst. Pre-school Teacher",
"Student/Teacher",
"Theater artist/ Teacher",
"Early child hood teacher",
"teacher / Administrator"), .N,]
```

```
## [1] 0
```

```r
procrasti_dt[ CurrentOccup == "Teacher", .N,]
```

```
## [1] 94
```

```r
# 2.d Fix the data types

str(procrasti_dt)
```

```
## Classes 'data.table' and 'data.frame':	4264 obs. of  61 variables:
##  $ Age         : num  67.5 45 19 37.5 28 23 67.5 37.5 24 45 ...
##  $ Gender      : chr  "Male" "Male" "Female" "Male" ...
##  $ Kids        : chr  "Yes Kids" "Yes Kids" "No Kids" "Yes Kids" ...
##  $ Education   : chr  "ma" "deg" "dip" "ma" ...
##  $ WorkStatus  : chr  "retired" "part-time" "student" "full-time" ...
##  $ AnnualIncome: int  25000 35000 NA 45000 35000 15000 NA 10000 250000 87500 ...
##  $ CurrentOccup: chr  NA NA NA NA ...
##  $ CurrPosYrs  : num  9 0 0 14 1 1 8 NA 2 14 ...
##  $ CurrPosMon  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ CommuSize   : chr  "Large-City" "Village" "Large Town" "Large Town" ...
##  $ CountryOfRes: chr  "El Salvador" "Bolivia" "Cyprus" "Czech Republic" ...
##  $ MaritalStat : chr  "Divorced" "Married" "Single" "Married" ...
##  $ NoOfSons    : int  0 1 0 0 0 0 0 0 0 2 ...
##  $ NoOfDaughtrs: int  5 1 0 1 0 0 0 0 0 0 ...
##  $ DP1WaTrivial: int  3 3 5 3 3 3 3 4 2 5 ...
##  $ DP2DelayAct : int  1 4 5 3 3 4 4 3 2 5 ...
##  $ DP3MakNoDeci: int  1 3 2 3 2 3 3 4 4 5 ...
##  $ DP4DelyDeci : int  1 3 3 3 1 2 2 4 4 5 ...
##  $ DP5PutOfDeci: int  1 3 3 3 1 2 2 3 4 5 ...
##  $ AIP1PayBilOT: int  1 3 5 2 1 2 3 4 3 3 ...
##  $ AIP2OnTimApp: int  1 1 4 1 1 5 1 4 3 3 ...
##  $ AIP3LayOTClo: int  1 4 4 4 3 5 1 4 3 5 ...
##  $ AIP4RunLater: int  1 3 5 3 3 5 2 4 4 3 ...
##  $ AIP5DonOnTim: int  1 3 5 5 2 5 3 4 4 5 ...
##  $ AIP6AttCours: int  1 4 5 3 2 3 3 2 2 1 ...
##  $ AIP7FFWaitLM: int  1 3 5 4 2 5 1 5 2 5 ...
##  $ AIP8GetDonTS: int  1 3 4 5 2 4 4 2 4 5 ...
##  $ AIP9NtGdMeDe: int  5 3 5 4 1 4 5 4 2 5 ...
##  $ AIP10RnOtTim: int  1 3 5 5 1 5 5 3 2 5 ...
##  $ AIP11ScDrApp: int  1 4 4 4 2 3 3 2 4 4 ...
##  $ AIP12IamPunc: int  1 2 3 3 1 5 2 4 4 4 ...
##  $ AIP13DoRoMai: int  1 2 5 4 2 4 3 3 4 4 ...
##  $ AIP14FExpLat: int  1 2 4 2 1 5 1 4 3 4 ...
##  $ AIP15PtLsMin: int  3 4 3 1 2 5 4 5 3 5 ...
##  $ GP1PrTskLate: int  1 4 5 4 4 5 4 5 3 5 ...
##  $ GP2MissEvnts: int  1 2 2 1 1 5 1 1 4 3 ...
##  $ GP3PlPrtyAdv: int  1 2 2 3 2 2 1 3 3 3 ...
##  $ GP4MgGetUpOT: int  1 2 4 3 4 5 1 4 4 1 ...
##  $ GP5MailLtrLt: int  1 2 3 2 5 4 1 3 2 5 ...
##  $ GP6RtPhPromt: int  1 2 1 3 2 4 2 3 2 5 ...
##  $ GP7DlyEsyJob: int  1 4 3 4 4 5 3 4 4 5 ...
##  $ GP8MkDesAsap: int  1 2 2 5 2 4 2 3 2 4 ...
##  $ GP9DlyStrWrk: int  1 4 5 4 4 4 4 4 4 5 ...
##  $ GP10RshFoTvl: int  1 2 4 1 1 3 1 4 1 3 ...
##  $ GP11PrWelGoO: int  5 3 5 3 2 4 4 3 5 5 ...
##  $ GP12WaTimOth: int  1 4 5 4 3 4 2 5 2 5 ...
##  $ GP13PySmBilI: int  1 2 3 3 2 3 3 2 3 4 ...
##  $ GP14RtRSVPIm: int  1 2 4 3 4 4 2 2 2 4 ...
##  $ GP15FnTskSnr: int  1 3 5 4 3 4 4 4 1 4 ...
##  $ GP16ShBDGfLM: int  1 4 2 4 2 4 3 4 5 5 ...
##  $ GP17ShEsItLM: int  1 3 3 3 3 4 1 3 5 3 ...
##  $ GP18AccInDay: int  5 3 5 4 2 4 4 4 1 5 ...
##  $ GP19IDoItTom: int  1 4 5 5 3 4 4 4 1 5 ...
##  $ GP20IFnAlTsk: int  5 4 4 1 4 4 2 4 3 5 ...
##  $ SWLS1LifCIdl: int  5 3 2 2 4 3 3 3 4 1 ...
##  $ SWLS2LiCnExl: int  5 4 2 4 4 2 4 3 4 4 ...
##  $ SWLS3SatisLf: int  5 4 2 2 4 4 3 3 5 2 ...
##  $ SWLS4GtImpTh: int  5 4 3 2 3 4 3 2 4 4 ...
##  $ SWLS5SamLfAg: int  5 3 4 2 4 3 2 3 4 1 ...
##  $ SlfConProCra: chr  "no" "yes" "yes" "yes" ...
##  $ OtrConProCra: chr  "no" "yes" "yes" "yes" ...
##  - attr(*, ".internal.selfref")=<externalptr> 
##  - attr(*, "index")= atomic  
##   ..- attr(*, "__CountryOfRes")= int  4029 4030 4031 4032 4033 4034 4035 4036 4037 4038 ...
##   ..- attr(*, "__CurrentOccup")= int  1 2 3 4 5 6 7 8 9 10 ...
```

```r
# All looks good now

# 2.e Create new Mean column for DP, AIP, GP, SWLS

# Create a Mean Column

procrasti_dt[ , DPMean := ((DP1WaTrivial + DP2DelayAct + DP3MakNoDeci + DP4DelyDeci + DP5PutOfDeci)/5), ]

procrasti_dt[ , AIPMean := ((AIP1PayBilOT + AIP2OnTimApp + AIP3LayOTClo + AIP4RunLater + AIP5DonOnTim +AIP6AttCours + AIP7FFWaitLM+AIP8GetDonTS + AIP9NtGdMeDe+ AIP10RnOtTim+ AIP11ScDrApp+ AIP12IamPunc+ AIP13DoRoMai+ AIP14FExpLat+AIP15PtLsMin )/15), ]

procrasti_dt[ , GPMean := ((GP1PrTskLate + GP2MissEvnts + GP3PlPrtyAdv + GP4MgGetUpOT + GP5MailLtrLt +GP6RtPhPromt + GP7DlyEsyJob+GP8MkDesAsap + GP9DlyStrWrk+ GP10RshFoTvl+ GP11PrWelGoO+ GP12WaTimOth+ GP13PySmBilI+ GP14RtRSVPIm+GP15FnTskSnr + GP16ShBDGfLM+GP17ShEsItLM +GP18AccInDay +GP19IDoItTom +GP20IFnAlTsk )/20), ]

procrasti_dt[ , SWLSMean := ((SWLS1LifCIdl + SWLS2LiCnExl + SWLS3SatisLf + SWLS4GtImpTh + SWLS5SamLfAg)/5), ]

#Check Resluts
#str(procrasti_dt)


# 3 Scrape the Human Development Index Online

library("rvest") # Great for grabbing and parsing HTML

#Pulling Human Development Index Online
url = 'https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Complete_list_of_countries'
webpage_hdit = read_html(url)

# Identifying HTML nodes
hdit_tables = html_nodes(webpage_hdit, 'table')
?html_nodes

#str(hdit_tables)
#head(hdit_tables, 25)

# 3.a Scrape the page and identify the table of the Complete Country List in order of crediting

#hdit_cntry_table <- html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)

#hdit_cntry_table <- as.data.table(html_table (hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE))

#hdit_cntry_table <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(1,2,3,4,5,6,7,8)])

hdit_cntry_table1 <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(1)])
hdit_cntry_table2 <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(2)])
hdit_cntry_table3 <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(3)])
hdit_cntry_table4 <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(4)])
hdit_cntry_table5 <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(5)])
hdit_cntry_table6 <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(6)])
hdit_cntry_table7 <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(7)])
hdit_cntry_table8 <- as.data.table(html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)[c(8)])

#str(hdit_cntry_table8)

#?html_table
#?as.data.table
#html_table(hdit_tables [c(4,5,7,8,10,11,13,14)], fill = TRUE)

#hdit_cntry_table <- rbind(hdit_cntry_table1,hdit_cntry_table2,hdit_cntry_table3,hdit_cntry_table4,hdit_cntry_table5,hdit_cntry_table6,hdit_cntry_table7,hdit_cntry_table8)

#str(hdit_cntry_table8)


#?do.call
# Converting to Data Frame
#hdit_df = data.frame(hdit_cntry_table , check.names = FALSE, check.rows = TRUE)
#?data.frame

#hdit_dt <- as.data.table(c(hdit_cntry_table1,hdit_cntry_table2) , as.is = TRUE)
#hdit_dt <- as.data.table(hdit_cntry_table1, as.is = TRUE)

#?data.table

#Clean up the table, remove all columns except country and HDI 2016 - these steps are in the nect section 3.a,b

#hdit_cntry_table1<- hdit_cntry_table1[, 3:4]
#hdit_cntry_table2<- hdit_cntry_table2[, 3:4]
#hdit_cntry_table3<- hdit_cntry_table3[, 3:4]
#hdit_cntry_table4<- hdit_cntry_table4[, 3:4]
#hdit_cntry_table5<- hdit_cntry_table5[, 3:4]
#hdit_cntry_table6<- hdit_cntry_table6[, 3:4]
#hdit_cntry_table7<- hdit_cntry_table7[, 3:4]
#hdit_cntry_table8<- hdit_cntry_table8[, 3:4]


# 3.a, b
#Clean up the table, remove all columns except country and HDI 2016
#Updating column names Country and HDI and adding Category column

hdit_cntry_table1<- hdit_cntry_table1[, 3:4]
names(hdit_cntry_table1)<- c("Country", "HDI")
hdit_cntry_table1<- hdit_cntry_table1[2:27]
hdit_cntry_table1[ , HDICategory := "Very High human development", ]


hdit_cntry_table2<- hdit_cntry_table2[, 3:4]
names(hdit_cntry_table2)<- c("Country", "HDI")
hdit_cntry_table2<- hdit_cntry_table2[2:26]
hdit_cntry_table2[ , HDICategory := "Very High human development", ]


hdit_cntry_table3<- hdit_cntry_table3[, 3:4]
names(hdit_cntry_table3)<- c("Country", "HDI")
hdit_cntry_table3<- hdit_cntry_table3[2:29]
hdit_cntry_table3[ , HDICategory := "High human development", ]


hdit_cntry_table4<- hdit_cntry_table4[, 3:4]
names(hdit_cntry_table4)<- c("Country", "HDI")
hdit_cntry_table4<- hdit_cntry_table4[2:29]
hdit_cntry_table4[ , HDICategory := "High human development", ]


hdit_cntry_table5<- hdit_cntry_table5[, 3:4]
names(hdit_cntry_table5)<- c("Country", "HDI")
hdit_cntry_table5<- hdit_cntry_table5[2:21]
hdit_cntry_table5[ , HDICategory := "Medium human development", ]


hdit_cntry_table6<- hdit_cntry_table6[, 3:4]
names(hdit_cntry_table6)<- c("Country", "HDI")
hdit_cntry_table6<- hdit_cntry_table6[2:22]
hdit_cntry_table6[ , HDICategory := "Medium human development", ]


hdit_cntry_table7<- hdit_cntry_table7[, 3:4]
names(hdit_cntry_table7)<- c("Country", "HDI")
hdit_cntry_table7<- hdit_cntry_table7[2:22]
hdit_cntry_table7[ , HDICategory := "Low human development", ]


hdit_cntry_table8<- hdit_cntry_table8[, 3:4]
names(hdit_cntry_table8)<- c("Country", "HDI")
hdit_cntry_table8<- hdit_cntry_table8[2:21]
hdit_cntry_table8[ , HDICategory := "Low human development", ]

#Merge 8 tables to one table

hdit_cntry_table <- rbind(hdit_cntry_table1,hdit_cntry_table2,hdit_cntry_table3,hdit_cntry_table4,hdit_cntry_table5,hdit_cntry_table6,hdit_cntry_table7,hdit_cntry_table8)

# Write to a csv file.
write.csv(hdit_cntry_table , file = "HDI_Country.csv", row.names = FALSE)

#str(hdit_cntry_table8)

# 3.c Merge 2 DT

merge_dt <- merge( procrasti_dt,hdit_cntry_table, by.x="CountryOfRes", by.y="Country", all=TRUE)

# 4.a Remove all under age 18

merge_dt <- merge_dt[ Age >18, , ]

#4.b
des_stats <- merge_dt[ , .(Age, AnnualIncome, HDI,DPMean,AIPMean,GPMean,SWLSMean), ]

hist(des_stats$Age, main = "AGE HIST", xlab = "AGE" )
```

![](CaseStudy2_InitialDraft_files/figure-html/procrastination-1.png)<!-- -->

```r
hist(des_stats$DPMean , main = "DPMean HIST", xlab = "DPMean" )
```

![](CaseStudy2_InitialDraft_files/figure-html/procrastination-2.png)<!-- -->

```r
?hist

nrow(des_stats$Age)
```

```
## NULL
```

```r
nrow(des_stats$AnnualIncome)
```

```
## NULL
```

```r
str(des_stats)
```

```
## Classes 'data.table' and 'data.frame':	4036 obs. of  7 variables:
##  $ Age         : num  23 55 37.5 67.5 25 45 67.5 45 55 55 ...
##  $ AnnualIncome: int  10000 55000 250000 67500 25000 15000 NA 87500 25000 87500 ...
##  $ HDI         : chr  NA NA NA NA ...
##  $ DPMean      : num  2 3 3.6 1.4 4 3.4 2.4 3.4 1.8 4.2 ...
##  $ AIPMean     : num  1.93 2.13 3.47 1.8 2.27 ...
##  $ GPMean      : num  2.15 3.05 4.1 2.1 3.3 3.55 2.55 3.1 2.35 3.7 ...
##  $ SWLSMean    : num  3.8 3.4 3 4.2 3 1 3.4 3.8 4 1 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
#TO CHECK NA COUNT
des_stats[, sum(is.na(Age)), ]
```

```
## [1] 0
```

```r
des_stats[, sum(is.na(AnnualIncome)), ]
```

```
## [1] 415
```

```r
des_stats[, sum(is.na(HDI)), ]
```

```
## [1] 192
```

```r
des_stats[, sum(is.na(DPMean)), ]
```

```
## [1] 0
```

```r
#4.c frequencies of variables - dplyr function

freq_gen <- summarise(group_by(merge_dt,Gender) , GenderFreq =n())
freq_gen <- freq_gen[2:3, ]

freq_WorkStatus <- summarise(group_by(merge_dt,WorkStatus) , WorkStatusFreq =n())
freq_WorkStatus <- freq_WorkStatus[2:6, ]

freq_CurrentOccup <- summarise(group_by(merge_dt,CurrentOccup) , CurrentOccupFreq =n())

str(merge_dt)
```

```
## Classes 'data.table' and 'data.frame':	4036 obs. of  67 variables:
##  $ CountryOfRes: chr  NA NA NA NA ...
##  $ Age         : num  23 55 37.5 67.5 25 45 67.5 45 55 55 ...
##  $ Gender      : chr  "Female" "Male" "Male" "Female" ...
##  $ Kids        : chr  "No Kids" "No Kids" "Yes Kids" "Yes Kids" ...
##  $ Education   : chr  "deg" "ltuni" "phd" "ma" ...
##  $ WorkStatus  : chr  "student" "retired" "full-time" "retired" ...
##  $ AnnualIncome: int  10000 55000 250000 67500 25000 15000 NA 87500 25000 87500 ...
##  $ CurrentOccup: chr  "Law clerk" "Farm Manager" "engineer" NA ...
##  $ CurrPosYrs  : num  0 8 4 0 3 5 0 13 17 0 ...
##  $ CurrPosMon  : int  8 2 0 0 0 8 0 0 0 0 ...
##  $ CommuSize   : chr  "Small City" "Large-City" "Large-City" "Medium-Sized" ...
##  $ MaritalStat : chr  "Divorced" "Married" "Married" "Married" ...
##  $ NoOfSons    : int  0 0 1 2 0 1 3 0 2 1 ...
##  $ NoOfDaughtrs: int  0 0 0 1 0 0 2 2 1 1 ...
##  $ DP1WaTrivial: int  3 3 4 2 4 4 4 4 3 4 ...
##  $ DP2DelayAct : int  2 3 4 2 4 4 1 4 2 4 ...
##  $ DP3MakNoDeci: int  3 3 4 1 4 3 1 3 2 4 ...
##  $ DP4DelyDeci : int  1 3 3 1 4 3 1 3 1 5 ...
##  $ DP5PutOfDeci: int  1 3 3 1 4 3 5 3 1 4 ...
##  $ AIP1PayBilOT: int  1 1 5 1 2 3 1 1 1 3 ...
##  $ AIP2OnTimApp: int  1 1 4 1 1 3 5 1 1 2 ...
##  $ AIP3LayOTClo: int  2 1 5 4 2 3 3 4 1 3 ...
##  $ AIP4RunLater: int  3 1 2 2 2 3 3 3 3 4 ...
##  $ AIP5DonOnTim: int  2 3 2 2 2 3 1 4 2 4 ...
##  $ AIP6AttCours: int  2 1 4 1 2 3 1 2 1 4 ...
##  $ AIP7FFWaitLM: int  2 3 2 1 2 3 1 3 1 4 ...
##  $ AIP8GetDonTS: int  2 3 5 2 2 3 1 2 1 4 ...
##  $ AIP9NtGdMeDe: int  2 3 2 2 2 3 1 4 1 3 ...
##  $ AIP10RnOtTim: int  3 3 1 1 4 4 2 4 2 4 ...
##  $ AIP11ScDrApp: int  2 3 4 2 4 3 1 5 2 5 ...
##  $ AIP12IamPunc: int  2 2 4 2 2 3 1 2 2 4 ...
##  $ AIP13DoRoMai: int  3 2 4 4 3 3 1 4 1 4 ...
##  $ AIP14FExpLat: int  1 1 3 1 1 2 1 1 1 4 ...
##  $ AIP15PtLsMin: int  1 4 5 1 3 4 2 3 1 5 ...
##  $ GP1PrTskLate: int  3 3 5 3 4 4 3 4 3 5 ...
##  $ GP2MissEvnts: int  1 3 4 2 4 3 1 3 3 3 ...
##  $ GP3PlPrtyAdv: int  5 2 4 2 4 4 1 3 1 4 ...
##  $ GP4MgGetUpOT: int  4 2 4 1 5 4 1 3 2 3 ...
##  $ GP5MailLtrLt: int  2 3 2 2 4 3 1 3 3 3 ...
##  $ GP6RtPhPromt: int  2 2 4 1 3 3 1 2 1 4 ...
##  $ GP7DlyEsyJob: int  2 4 4 1 4 3 4 4 3 3 ...
##  $ GP8MkDesAsap: int  2 3 3 1 4 4 2 2 2 4 ...
##  $ GP9DlyStrWrk: int  2 4 4 3 5 2 3 4 3 3 ...
##  $ GP10RshFoTvl: int  1 2 5 1 1 3 1 1 1 3 ...
##  $ GP11PrWelGoO: int  2 4 5 4 2 4 2 3 4 4 ...
##  $ GP12WaTimOth: int  3 4 4 3 4 4 3 4 3 4 ...
##  $ GP13PySmBilI: int  1 2 5 4 4 3 5 3 2 3 ...
##  $ GP14RtRSVPIm: int  1 4 5 3 2 3 2 4 2 4 ...
##  $ GP15FnTskSnr: int  2 3 5 2 2 4 3 3 3 4 ...
##  $ GP16ShBDGfLM: int  2 4 5 2 3 4 5 4 3 4 ...
##  $ GP17ShEsItLM: int  2 2 1 2 3 3 5 3 3 3 ...
##  $ GP18AccInDay: int  2 3 5 3 2 4 2 2 2 5 ...
##  $ GP19IDoItTom: int  2 3 3 1 2 5 1 4 2 4 ...
##  $ GP20IFnAlTsk: int  2 4 5 1 4 4 5 3 1 4 ...
##  $ SWLS1LifCIdl: int  4 4 3 4 2 1 3 4 4 1 ...
##  $ SWLS2LiCnExl: int  4 4 3 5 3 1 4 4 5 1 ...
##  $ SWLS3SatisLf: int  4 4 3 4 3 1 4 4 4 1 ...
##  $ SWLS4GtImpTh: int  4 4 3 4 4 1 5 4 4 1 ...
##  $ SWLS5SamLfAg: int  3 1 3 4 3 1 1 3 3 1 ...
##  $ SlfConProCra: chr  "no" "yes" "yes" "no" ...
##  $ OtrConProCra: chr  "no" "yes" "no" "no" ...
##  $ DPMean      : num  2 3 3.6 1.4 4 3.4 2.4 3.4 1.8 4.2 ...
##  $ AIPMean     : num  1.93 2.13 3.47 1.8 2.27 ...
##  $ GPMean      : num  2.15 3.05 4.1 2.1 3.3 3.55 2.55 3.1 2.35 3.7 ...
##  $ SWLSMean    : num  3.8 3.4 3 4.2 3 1 3.4 3.8 4 1 ...
##  $ HDI         : chr  NA NA NA NA ...
##  $ HDICategory : chr  NA NA NA NA ...
##  - attr(*, "sorted")= chr "CountryOfRes"
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
#?summarise

#4.d Count Participiants per country desc

freq_parti <- summarise(group_by(merge_dt,CountryOfRes) , NoOfParticipants =n())
freq_parti <- data.table(freq_parti)
freq_parti <- freq_parti [ order(-NoOfParticipants)]

?order
```

```
## Help on topic 'order' was found in the following packages:
## 
##   Package               Library
##   data.table            /Library/Frameworks/R.framework/Versions/3.4/Resources/library
##   base                  /Library/Frameworks/R.framework/Resources/library
## 
## 
## Using the first match ...
```

```r
#4.e Count of procra_yes_yes and procra_no_no

nrow(merge_dt[ SlfConProCra == "yes" & OtrConProCra == "yes" , ,] )
```

```
## [1] 2358
```

```r
nrow(merge_dt[ SlfConProCra == "no" & OtrConProCra == "no" , ,] )
```

```
## [1] 482
```

```r
#nrow(merge_dt[ SlfConProCra == "yes" & OtrConProCra == "no" , ,] )
#nrow(merge_dt[ SlfConProCra == "no" & OtrConProCra == "yes" , ,] )
#nrow(merge_dt[ SlfConProCra == "" & OtrConProCra == "" , ,] )
#nrow(merge_dt[ SlfConProCra == "" & OtrConProCra == "0" , ,] )

# 5.a 
# 5.b

#Create a colorful bar chart that shows the top 15 nations avg HDI score

#head(merge_dt)

# DP
top_15_countries_dp <- merge_dt [ , .(CountryOfRes, HDICategory, DPMean), ]

top_15_countries_dp <- head(unique(na.omit(top_15_countries_dp[order(-DPMean)]), by = c("CountryOfRes")), 15)

ggplot(top_15_countries_dp, aes(reorder(CountryOfRes, DPMean) , DPMean))+
  geom_bar(aes(fill = HDICategory), stat = "identity")+
  coord_flip() +
  ggtitle(" Average DP Score Per Country")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Country")+
  ylab ("DP Mean")
```

![](CaseStudy2_InitialDraft_files/figure-html/procrastination-3.png)<!-- -->

```r
# AIP
top_15_countries_aip <- merge_dt [ , .(CountryOfRes, HDICategory, AIPMean), ]

top_15_countries_aip <- na.omit(top_15_countries_aip[order(-AIPMean)])

top_15_countries_aip <- head(unique(top_15_countries_aip, by = c("CountryOfRes")), 15)


ggplot(top_15_countries_aip, aes(reorder(CountryOfRes, AIPMean) , AIPMean))+
  geom_bar(aes(fill = HDICategory), stat = "identity")+
  coord_flip() +
  ggtitle(" Average AIP Score Per Country")+
  theme(plot.title = element_text(hjust = 0.25))+
  xlab("Country")+
  ylab ("AIP Mean")
```

![](CaseStudy2_InitialDraft_files/figure-html/procrastination-4.png)<!-- -->

```r
#5.c GP Mean
top_15_countries_gp <- merge_dt [ , .(CountryOfRes, HDICategory, GPMean), ]

top_15_countries_gp <- na.omit(top_15_countries_gp[order(-GPMean)])

top_15_countries_gp <- head(unique(top_15_countries_gp, by = c("CountryOfRes")), 15)


ggplot(top_15_countries_gp, aes(reorder(CountryOfRes, GPMean) , GPMean))+
  geom_bar(aes(fill = HDICategory), stat = "identity")+
  coord_flip() +
  ggtitle(" Average GP Score Per Country")+
  theme(plot.title = element_text(hjust = 0.25))+
  xlab("Country")+
  ylab ("GP Mean")
```

![](CaseStudy2_InitialDraft_files/figure-html/procrastination-5.png)<!-- -->

```r
# Below countries appear in AIP and DP score top 15 lists
# India, Australia, Canada, Belgium, Brazil, Finland, France, Ireland

# 5.d Relationship Age and Income

#Linear reg

regressor = lm(formula = AnnualIncome ~ Age, data=merge_dt )
regressor
```

```
## 
## Call:
## lm(formula = AnnualIncome ~ Age, data = merge_dt)
## 
## Coefficients:
## (Intercept)          Age  
##        1951         1497
```

```r
summary(regressor)
```

```
## 
## Call:
## lm(formula = AnnualIncome ~ Age, data = merge_dt)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -111693  -28861  -14370   11139  213623 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1950.98    2592.53   0.753    0.452    
## Age          1496.77      63.23  23.671   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 51490 on 3619 degrees of freedom
##   (415 observations deleted due to missingness)
## Multiple R-squared:  0.1341,	Adjusted R-squared:  0.1338 
## F-statistic: 560.3 on 1 and 3619 DF,  p-value: < 2.2e-16
```

```r
#predict
y_pred = predict(regressor, newdata = merge_dt )
#y_pred


ggplot() +
  geom_point(data = merge_dt, aes(x= merge_dt$Age, y= merge_dt$AnnualIncome),
             colour='red', na.rm = TRUE,size = .75)+
  geom_line(aes(x= merge_dt$Age,y= predict(regressor, newdata = merge_dt )),
            colour='blue',na.rm = TRUE)+
  ggtitle('Age vs Anual Income')+
  xlab('Age')+
  ylab('Income')
```

![](CaseStudy2_InitialDraft_files/figure-html/procrastination-6.png)<!-- -->

```r
# 5.e Relationship Life Satisfaction and HDI
#Linear reg

regressor = lm(formula =  SWLS3SatisLf ~ HDI , data=merge_dt )
regressor
```

```
## 
## Call:
## lm(formula = SWLS3SatisLf ~ HDI, data = merge_dt)
## 
## Coefficients:
## (Intercept)     HDI0.550     HDI0.555     HDI0.556     HDI0.579  
##   2.000e+00    2.000e+00    1.000e+00    1.000e+00    1.500e+00  
##    HDI0.624     HDI0.638     HDI0.645     HDI0.647     HDI0.666  
##   9.487e-01    2.000e+00    2.000e+00    1.000e+00    6.667e-01  
##    HDI0.674     HDI0.680     HDI0.682     HDI0.683     HDI0.691  
##   2.000e+00    3.000e+00    7.273e-01    2.000e+00    1.000e+00  
##    HDI0.698     HDI0.722     HDI0.730     HDI0.738     HDI0.739  
##  -1.274e-12    1.000e+00    3.000e+00    1.000e+00    6.667e-01  
##    HDI0.740     HDI0.743     HDI0.745     HDI0.748     HDI0.754  
##   1.000e+00    2.000e+00    2.333e+00   -1.640e-12    5.000e-01  
##    HDI0.762     HDI0.764     HDI0.766     HDI0.767     HDI0.774  
##   1.500e+00    1.000e+00    1.000e+00    6.364e-01    1.000e+00  
##    HDI0.788     HDI0.789     HDI0.792     HDI0.794     HDI0.795  
##  -1.000e+00    1.000e+00   -1.780e-12   -1.483e-12    1.500e+00  
##    HDI0.802     HDI0.804     HDI0.827     HDI0.836     HDI0.843  
##   4.000e-01    1.000e+00    1.714e+00    1.000e+00    2.857e-01  
##    HDI0.847     HDI0.848     HDI0.855     HDI0.856     HDI0.858  
##   1.333e+00    2.000e+00    1.000e+00    5.000e-01    2.000e+00  
##    HDI0.865     HDI0.866     HDI0.878     HDI0.884     HDI0.887  
##   3.000e+00    8.000e-01    1.333e+00    4.615e-01    6.452e-01  
##    HDI0.890     HDI0.893     HDI0.895     HDI0.896     HDI0.897  
##   1.000e+00    6.667e-01    5.000e-01    5.556e-01    6.923e-01  
##    HDI0.898     HDI0.901     HDI0.903     HDI0.909     HDI0.913  
##   1.000e+00    1.500e+00    8.462e-01    6.667e-01    8.667e-01  
##    HDI0.915     HDI0.917     HDI0.920     HDI0.921     HDI0.923  
##   1.333e+00    8.571e-01    1.193e+00   -1.332e-12    6.842e-01  
##    HDI0.924     HDI0.925     HDI0.926     HDI0.939     HDI0.949  
##   1.556e+00    1.077e+00    7.778e-01    8.829e-01    1.071e+00
```

```r
summary(regressor)
```

```
## 
## Call:
## lm(formula = SWLS3SatisLf ~ HDI, data = merge_dt)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.5556 -0.9487 -0.1929  0.8071  2.3548 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.000e+00  5.651e-01   3.539 0.000406 ***
## HDI0.550     2.000e+00  1.264e+00   1.583 0.113567    
## HDI0.555     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.556     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.579     1.500e+00  9.788e-01   1.532 0.125490    
## HDI0.624     9.487e-01  5.794e-01   1.637 0.101642    
## HDI0.638     2.000e+00  1.264e+00   1.583 0.113567    
## HDI0.645     2.000e+00  1.264e+00   1.583 0.113567    
## HDI0.647     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.666     6.667e-01  6.525e-01   1.022 0.307013    
## HDI0.674     2.000e+00  1.264e+00   1.583 0.113567    
## HDI0.680     3.000e+00  1.264e+00   2.374 0.017642 *  
## HDI0.682     7.273e-01  6.599e-01   1.102 0.270501    
## HDI0.683     2.000e+00  1.264e+00   1.583 0.113567    
## HDI0.691     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.698    -1.274e-12  1.264e+00   0.000 1.000000    
## HDI0.722     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.730     3.000e+00  1.264e+00   2.374 0.017642 *  
## HDI0.738     1.000e+00  6.525e-01   1.532 0.125490    
## HDI0.739     6.667e-01  8.632e-01   0.772 0.439989    
## HDI0.740     1.000e+00  7.992e-01   1.251 0.210918    
## HDI0.743     2.000e+00  9.788e-01   2.043 0.041092 *  
## HDI0.745     2.333e+00  8.632e-01   2.703 0.006902 ** 
## HDI0.748    -1.640e-12  1.264e+00   0.000 1.000000    
## HDI0.754     5.000e-01  6.191e-01   0.808 0.419324    
## HDI0.762     1.500e+00  6.525e-01   2.299 0.021576 *  
## HDI0.764     1.000e+00  9.788e-01   1.022 0.307013    
## HDI0.766     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.767     6.364e-01  6.599e-01   0.964 0.334951    
## HDI0.774     1.000e+00  9.788e-01   1.022 0.307013    
## HDI0.788    -1.000e+00  1.264e+00  -0.791 0.428781    
## HDI0.789     1.000e+00  7.992e-01   1.251 0.210918    
## HDI0.792    -1.780e-12  1.264e+00   0.000 1.000000    
## HDI0.794    -1.483e-12  8.632e-01   0.000 1.000000    
## HDI0.795     1.500e+00  7.992e-01   1.877 0.060611 .  
## HDI0.802     4.000e-01  7.582e-01   0.528 0.597824    
## HDI0.804     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.827     1.714e+00  7.084e-01   2.420 0.015572 *  
## HDI0.836     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.843     2.857e-01  7.084e-01   0.403 0.686738    
## HDI0.847     1.333e+00  7.296e-01   1.828 0.067692 .  
## HDI0.848     2.000e+00  1.264e+00   1.583 0.113567    
## HDI0.855     1.000e+00  7.582e-01   1.319 0.187269    
## HDI0.856     5.000e-01  7.992e-01   0.626 0.531596    
## HDI0.858     2.000e+00  9.788e-01   2.043 0.041092 *  
## HDI0.865     3.000e+00  1.264e+00   2.374 0.017642 *  
## HDI0.866     8.000e-01  6.687e-01   1.196 0.231603    
## HDI0.878     1.333e+00  8.632e-01   1.545 0.122530    
## HDI0.884     4.615e-01  6.462e-01   0.714 0.475151    
## HDI0.887     6.452e-01  5.831e-01   1.107 0.268578    
## HDI0.890     1.000e+00  7.296e-01   1.371 0.170555    
## HDI0.893     6.667e-01  8.632e-01   0.772 0.439989    
## HDI0.895     5.000e-01  6.525e-01   0.766 0.443584    
## HDI0.896     5.556e-01  6.792e-01   0.818 0.413424    
## HDI0.897     6.923e-01  6.462e-01   1.071 0.284106    
## HDI0.898     1.000e+00  1.264e+00   0.791 0.428781    
## HDI0.901     1.500e+00  9.788e-01   1.532 0.125490    
## HDI0.903     8.462e-01  6.462e-01   1.309 0.190493    
## HDI0.909     6.667e-01  5.715e-01   1.167 0.243450    
## HDI0.913     8.667e-01  6.360e-01   1.363 0.173076    
## HDI0.915     1.333e+00  6.525e-01   2.043 0.041092 *  
## HDI0.917     8.571e-01  7.084e-01   1.210 0.226374    
## HDI0.920     1.193e+00  5.655e-01   2.109 0.034972 *  
## HDI0.921    -1.332e-12  1.264e+00   0.000 1.000000    
## HDI0.923     6.842e-01  6.218e-01   1.100 0.271213    
## HDI0.924     1.556e+00  6.248e-01   2.490 0.012823 *  
## HDI0.925     1.077e+00  6.462e-01   1.666 0.095706 .  
## HDI0.926     7.778e-01  5.957e-01   1.306 0.191739    
## HDI0.939     8.829e-01  5.752e-01   1.535 0.124894    
## HDI0.949     1.071e+00  6.408e-01   1.672 0.094595 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.13 on 3774 degrees of freedom
##   (192 observations deleted due to missingness)
## Multiple R-squared:  0.03558,	Adjusted R-squared:  0.01794 
## F-statistic: 2.018 on 69 and 3774 DF,  p-value: 1.659e-06
```

```r
#predict
y_pred = predict(regressor, newdata = merge_dt )
#y_pred


ggplot() +
  geom_point(data = merge_dt, aes(x= merge_dt$HDI, y= merge_dt$SWLS3SatisLf),
             colour='red', na.rm = TRUE,size = .75)+
  geom_line(aes(x= merge_dt$HDI,y= predict(regressor, newdata = merge_dt )),
            colour='blue',na.rm = TRUE)+
  ggtitle('Life Satisfaction vs HDI')+
  xlab('HDI')+
  ylab('Life Satisfaction')
```

![](CaseStudy2_InitialDraft_files/figure-html/procrastination-7.png)<!-- -->

```r
# Bar plot HDI Category

merge_dt_hdi_cat <- data.table(na.omit(merge_dt[ , .(SWLS3SatisLf,HDICategory ), ]))

ggplot(data = merge_dt_hdi_cat, aes(SWLS3SatisLf , HDICategory))+
  geom_bar(aes(fill = HDICategory), stat = "identity")+
  #coord_flip() +
  ggtitle(" HDI Category")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Life Satisfaction")+
  ylab ("HDI Category")
```

![](CaseStudy2_InitialDraft_files/figure-html/procrastination-8.png)<!-- -->

```r
# 6.a
# Write CSV in R
write.csv(hdit_cntry_table , file = "HDI_Country.csv", row.names = FALSE)

# 6.b
# Write CSV in R
write.csv(merge_dt , file = "Procrastination_HDI.csv", row.names = FALSE)

# 6.c

# Adding HDI to 3.c data table
top_15_countries_dp_hdi <- merge_dt [ , .(CountryOfRes, HDICategory, DPMean, HDI), ]
top_15_countries_dp_hdi <- head(unique(na.omit(top_15_countries_dp_hdi[order(-DPMean)]), by = c("CountryOfRes")), 15)

top_15_countries_gp_hdi <- merge_dt [ , .(CountryOfRes, HDICategory, GPMean, HDI), ]
top_15_countries_gp_hdi <- head(unique(na.omit(top_15_countries_gp_hdi[order(-GPMean)]), by = c("CountryOfRes")), 15)

# Write CSV in R
write.csv(top_15_countries_dp_hdi , file = "Top_15_HDI_DP.csv", row.names = FALSE)
write.csv(top_15_countries_gp_hdi , file = "Top_15_HDI_GP.csv", row.names = FALSE)

# 6.d
# Codebook.
```



---
title: "Employment Study(Case Study 2)"
author: "Joe Schueder/Thejas Prasad"
date: "November 24, 2017"
output: 
  html_document:
    keep_md: true
---



#####Prepared by Joe Schueder and Thejas Prasar for "WAM (We are Motivated), Inc.

WAM Inc. would like an analysis of where to place their next set of employment training centers to market job productivity training.  The primary criteria that WAM uses when creating new centers is motivation levels/procrastination levels, income averages, and education levels.  They are also interested in how well developed the country of a potential center is.

The primary goal of this analysis was to understand the current climate for employment training centers around the world. This was done by analyzing employee data from several countries which included relevant procrastination scores, education, occupation, and income levels.  This data was combined with the internationally recognized HDI index.

The analysis is shown below. 

Data Preparation to read in and clean data was done.


```r
#1a
#getwd()
#setwd("C:/Users/jjschued/Documents")
procrast <- read.csv(file = "Procrastination.csv", sep=",", header=TRUE)

#1a
# number of rows and columns
dim(procrast)
```

```
## [1] 4264   61
```

```r
#1b column names

#code below is for viewing all column names befoe re-lableing
#colnames(procrast)

#relabling column names for readability
colnames(procrast) <- c("Age","Gender","Kids","Edu","WorkStatus","AnnualIncome","CurrentOccup","Timeinposyr","Timeinposmn","Communitysize","Countryres","MaritalStatus","Numsons","Numdaughters","DP1","DP2","DP3","DP4","DP5","AIP1","AIP2","AIP3","AIP4","AIP5","AIP6","AIP7","AIP8","AIP9","AIP10","AIP11","AIP12","AIP13","AIP14","AIP15","GP1","GP2","GP3","GP4","GP5","GP6","GP7","GP8","GP9","GP10","GP11","GP12","GP13","GP14","GP15","GP16","GP17","GP18","GP19","GP20","SWLS1","SWLS2","SWLS3","SWLS4","SWLS5","UCONSPRO","OTHCONSPRO")

#2c i get time in service out of scientific notation
procrast$Timeinposyr <- as.integer(format(procrast$Timeinposyr, scientific = F))
```

```
## Warning: NAs introduced by coercion
```

```r
# Turning 999 into NA
procrast[,"Timeinposyr"][procrast[,"Timeinposyr"]==999] <- NA
#2 c ii converting Male and Female into numeric representation
procrast$Numsons <- as.character(procrast$Numsons)

procrast$Numsons[procrast$Numsons == "Male"] <- "1"
procrast$Numsons[procrast$Numsons == "Female"] <- "2"
#2ciii
#get rid of 0 and blank and convert them to NA
procrast$Countryres <- as.character(procrast$Countryres)
procrast$Countryres[procrast$Countryres == "0"] <- "NA"
procrast$Countryres[procrast$Countryres == " "] <- "NA"

#2civ
#fix the occupation column NAs blanks, group
procrast$CurrentOccup <- as.character(procrast$CurrentOccup)
procrast$CurrentOccup[procrast$CurrentOccup == "0"] <- "NA"
procrast$CurrentOccup[procrast$CurrentOccup == "please specify"] <- "NA"
procrast$CurrentOccup = toupper(procrast$CurrentOccup)
#grouping like together now
procrast$CurrentOccup <- gsub("TEACHER.*","TEACHER", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("TECH.*","TECHNICAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("WRIT*","WRITER", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("STUDENT.*","STUDENT", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("*.STUDENT","STUDENT", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("SYSTEM.*","TECHNICAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("SOFTWARE.*","TECHNICAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("SELF.*","SELF EMPLOYED", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("SALES.*","SALES", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("RESEARCH.*","RESEARCH", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("REAL ESTATE.*","REAL ESTATE", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("REALTOR.*","REAL ESTATE", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("PROGRAMMER.*","TECHNICAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("IT .*","TECHNICAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("COMPUTER.*","TECHNICAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("DIRECTOR.*","DIRECTOR", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("FINANC.*","FINANCIAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("INSURANCE.*","INSURANCE", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("INFORMATION.*","TECHNICAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("MEDICAL.*","MEDICAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("LEGAL.*","LEGAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("LAW.*","LEGAL", procrast$CurrentOccup)
procrast$CurrentOccup <- gsub("OFFICE.*","OFFICE", procrast$CurrentOccup)
#code below can be used to view all occupations
# kable(unique(procrast$CurrentOccup), "html", padding = 2, longtable = TRUE) %>%
# kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

#2d CHECK FIELD TYPES nothing wrong for now will convert later if needed
#code commendted below can be uncommented to view data typess
#str(procrast)

#2e Get the means of the procrastination indexes

procrast$dpmean <- (procrast$DP1 + procrast$DP2 + procrast$DP3 + procrast$DP4 + procrast$DP5)/5

procrast$aipmean <- (procrast$AIP1 + procrast$AIP2 +procrast$AIP3 +procrast$AIP4 +procrast$AIP5 +procrast$AIP6 +procrast$AIP7 +procrast$AIP8 +procrast$AIP9 +procrast$AIP10 +procrast$AIP11 +procrast$AIP12 +procrast$AIP13 +procrast$AIP14 + procrast$AIP15) /15

procrast$gpmean <- (procrast$GP1 + procrast$GP2 + procrast$GP3 + procrast$GP4 + procrast$GP5 + procrast$GP6 + procrast$GP7 + procrast$GP8 + procrast$GP9 + procrast$GP10 + procrast$GP11 + procrast$GP12 + procrast$GP13 + procrast$GP14 + procrast$GP15 + procrast$GP16 + procrast$GP17 + procrast$GP18 + procrast$GP19 + procrast$GP20)/20

procrast$swlsmean <- (procrast$SWLS1 + procrast$SWLS2 + procrast$SWLS3 + procrast$SWLS4 + procrast$SWLS5)/5

#code below for viewing table contents
kable(head(procrast), "html", padding = 2, longtable = TRUE) %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<thead><tr>
<th style="text-align:right;"> Age </th>
   <th style="text-align:left;"> Gender </th>
   <th style="text-align:left;"> Kids </th>
   <th style="text-align:left;"> Edu </th>
   <th style="text-align:left;"> WorkStatus </th>
   <th style="text-align:right;"> AnnualIncome </th>
   <th style="text-align:left;"> CurrentOccup </th>
   <th style="text-align:right;"> Timeinposyr </th>
   <th style="text-align:right;"> Timeinposmn </th>
   <th style="text-align:left;"> Communitysize </th>
   <th style="text-align:left;"> Countryres </th>
   <th style="text-align:left;"> MaritalStatus </th>
   <th style="text-align:left;"> Numsons </th>
   <th style="text-align:right;"> Numdaughters </th>
   <th style="text-align:right;"> DP1 </th>
   <th style="text-align:right;"> DP2 </th>
   <th style="text-align:right;"> DP3 </th>
   <th style="text-align:right;"> DP4 </th>
   <th style="text-align:right;"> DP5 </th>
   <th style="text-align:right;"> AIP1 </th>
   <th style="text-align:right;"> AIP2 </th>
   <th style="text-align:right;"> AIP3 </th>
   <th style="text-align:right;"> AIP4 </th>
   <th style="text-align:right;"> AIP5 </th>
   <th style="text-align:right;"> AIP6 </th>
   <th style="text-align:right;"> AIP7 </th>
   <th style="text-align:right;"> AIP8 </th>
   <th style="text-align:right;"> AIP9 </th>
   <th style="text-align:right;"> AIP10 </th>
   <th style="text-align:right;"> AIP11 </th>
   <th style="text-align:right;"> AIP12 </th>
   <th style="text-align:right;"> AIP13 </th>
   <th style="text-align:right;"> AIP14 </th>
   <th style="text-align:right;"> AIP15 </th>
   <th style="text-align:right;"> GP1 </th>
   <th style="text-align:right;"> GP2 </th>
   <th style="text-align:right;"> GP3 </th>
   <th style="text-align:right;"> GP4 </th>
   <th style="text-align:right;"> GP5 </th>
   <th style="text-align:right;"> GP6 </th>
   <th style="text-align:right;"> GP7 </th>
   <th style="text-align:right;"> GP8 </th>
   <th style="text-align:right;"> GP9 </th>
   <th style="text-align:right;"> GP10 </th>
   <th style="text-align:right;"> GP11 </th>
   <th style="text-align:right;"> GP12 </th>
   <th style="text-align:right;"> GP13 </th>
   <th style="text-align:right;"> GP14 </th>
   <th style="text-align:right;"> GP15 </th>
   <th style="text-align:right;"> GP16 </th>
   <th style="text-align:right;"> GP17 </th>
   <th style="text-align:right;"> GP18 </th>
   <th style="text-align:right;"> GP19 </th>
   <th style="text-align:right;"> GP20 </th>
   <th style="text-align:right;"> SWLS1 </th>
   <th style="text-align:right;"> SWLS2 </th>
   <th style="text-align:right;"> SWLS3 </th>
   <th style="text-align:right;"> SWLS4 </th>
   <th style="text-align:right;"> SWLS5 </th>
   <th style="text-align:left;"> UCONSPRO </th>
   <th style="text-align:left;"> OTHCONSPRO </th>
   <th style="text-align:right;"> dpmean </th>
   <th style="text-align:right;"> aipmean </th>
   <th style="text-align:right;"> gpmean </th>
   <th style="text-align:right;"> swlsmean </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:right;"> 67.5 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> Yes Kids </td>
   <td style="text-align:left;"> ma </td>
   <td style="text-align:left;"> retired </td>
   <td style="text-align:right;"> 25000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Large-City </td>
   <td style="text-align:left;"> El Salvador </td>
   <td style="text-align:left;"> Divorced </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 1.400000 </td>
   <td style="text-align:right;"> 1.60 </td>
   <td style="text-align:right;"> 5.0 </td>
  </tr>
<tr>
<td style="text-align:right;"> 45.0 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> Yes Kids </td>
   <td style="text-align:left;"> deg </td>
   <td style="text-align:left;"> part-time </td>
   <td style="text-align:right;"> 35000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Village </td>
   <td style="text-align:left;"> Bolivia </td>
   <td style="text-align:left;"> Married </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 2.933333 </td>
   <td style="text-align:right;"> 2.90 </td>
   <td style="text-align:right;"> 3.6 </td>
  </tr>
<tr>
<td style="text-align:right;"> 19.0 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> No Kids </td>
   <td style="text-align:left;"> dip </td>
   <td style="text-align:left;"> student </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Large Town </td>
   <td style="text-align:left;"> Cyprus </td>
   <td style="text-align:left;"> Single </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:right;"> 3.6 </td>
   <td style="text-align:right;"> 4.400000 </td>
   <td style="text-align:right;"> 3.60 </td>
   <td style="text-align:right;"> 2.6 </td>
  </tr>
<tr>
<td style="text-align:right;"> 37.5 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> Yes Kids </td>
   <td style="text-align:left;"> ma </td>
   <td style="text-align:left;"> full-time </td>
   <td style="text-align:right;"> 45000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Large Town </td>
   <td style="text-align:left;"> Czech Republic </td>
   <td style="text-align:left;"> Married </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 3.333333 </td>
   <td style="text-align:right;"> 3.20 </td>
   <td style="text-align:right;"> 2.4 </td>
  </tr>
<tr>
<td style="text-align:right;"> 28.0 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> No Kids </td>
   <td style="text-align:left;"> deg </td>
   <td style="text-align:left;"> full-time </td>
   <td style="text-align:right;"> 35000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Village </td>
   <td style="text-align:left;"> Czech Republic </td>
   <td style="text-align:left;"> Single </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 1.733333 </td>
   <td style="text-align:right;"> 2.85 </td>
   <td style="text-align:right;"> 3.8 </td>
  </tr>
<tr>
<td style="text-align:right;"> 23.0 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> No Kids </td>
   <td style="text-align:left;"> deg </td>
   <td style="text-align:left;"> full-time </td>
   <td style="text-align:right;"> 15000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Small Town </td>
   <td style="text-align:left;"> Czech Republic </td>
   <td style="text-align:left;"> Single </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 4.333333 </td>
   <td style="text-align:right;"> 4.00 </td>
   <td style="text-align:right;"> 3.2 </td>
  </tr>
</tbody>
</table>

```r
## 3a Getting data about human development index from wikipedia

URL <- 'https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Complete_list_of_countries'

#very high index countries
  HDIURL <- URL %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/div[5]/table') %>%
  html_table(fill = TRUE)
  HDITABVH <- HDIURL[[1]]

#labeling useful columns  
colnames(HDITABVH)[1:5] <- c("Rank", "RankChng", "Country", "HDI", "HDICHNG")

#limiting columns and categorizing this table
HDITABVHS <- HDITABVH[1:5]

HDITABVHS$HDICAT <- "Very High"

#getting rid of extra rows
HDITABVHS <- HDITABVHS[-c(1, 2, 3, 30, 31), ]

#high index countries
  HDIURL <- URL %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/div[6]/table') %>%
  html_table(fill = TRUE)
  HDITABH <- HDIURL[[1]]

#change column names
  
colnames(HDITABH)[1:5] <- c("Rank", "RankChng", "Country", "HDI", "HDICHNG")
HDITABHS <- HDITABH[1:5]
HDITABHS$HDICAT <- "High"
  
#getting rid of extra rows including one labled world which is not a country
HDITABHS <- HDITABHS[-c(1, 2, 3, 32, 33, 56), ]

#medium index countries
  HDIURL <- URL %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/div[7]/table') %>%
  html_table(fill = TRUE)
  HDITABM <- HDIURL[[1]]
colnames(HDITABM)[1:5] <- c("Rank", "RankChng", "Country", "HDI", "HDICHNG")
  
HDITABMS <- HDITABM[1:5]
HDITABMS$HDICAT <- "Medium"

#getting rid of extra rows
HDITABMS <- HDITABMS[-c(1, 2, 3, 24, 25), ]

#low index countries
  HDIURL <- URL %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/div[8]/table') %>%
  html_table(fill=TRUE)
  HDITABL <- HDIURL[[1]]
  
  colnames(HDITABL)[1:5] <- c("Rank", "RankChng", "Country", "HDI", "HDICHNG")
  HDITABLS <- HDITABL[1:5]
  HDITABLS$HDICAT <- "Low"
  
#getting rid of extra rows
HDITABLS <- HDITABLS[-c(1, 2, 3, 25, 26), ]
  
# The commented code below was used to explore the data to eliminate bad rows
# kable(HDITABVHS, "html", padding = 2, longtable = TRUE, row.names = TRUE) %>%
# kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
# 
# kable(HDITABHS, "html", padding = 2, longtable = TRUE, row.names = TRUE) %>%
# kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
# 
# kable(HDITABMS, "html", padding = 2, longtable = TRUE, row.names = TRUE) %>%
# kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
# 
# kable(HDITABLS, "html", padding = 2, longtable = TRUE, row.names = TRUE) %>%
# kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
 
#3b merge all the tables together
hdicomb <- rbind(HDITABVHS, HDITABHS, HDITABMS, HDITABLS)

kable(hdicomb, "html", padding = 2, longtable = TRUE, row.names = TRUE) %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<thead><tr>
<th style="text-align:left;">   </th>
   <th style="text-align:left;"> Rank </th>
   <th style="text-align:left;"> RankChng </th>
   <th style="text-align:left;"> Country </th>
   <th style="text-align:left;"> HDI </th>
   <th style="text-align:left;"> HDICHNG </th>
   <th style="text-align:left;"> HDICAT </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Norway </td>
   <td style="text-align:left;"> 0.949 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Australia </td>
   <td style="text-align:left;"> 0.939 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Switzerland </td>
   <td style="text-align:left;"> 0.939 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Germany </td>
   <td style="text-align:left;"> 0.926 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Denmark </td>
   <td style="text-align:left;"> 0.925 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Singapore </td>
   <td style="text-align:left;"> 0.925 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Netherlands </td>
   <td style="text-align:left;"> 0.924 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Ireland </td>
   <td style="text-align:left;"> 0.923 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Iceland </td>
   <td style="text-align:left;"> 0.921 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Canada </td>
   <td style="text-align:left;"> 0.920 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> United States </td>
   <td style="text-align:left;"> 0.920 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Hong Kong </td>
   <td style="text-align:left;"> 0.917 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> New Zealand </td>
   <td style="text-align:left;"> 0.915 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Sweden </td>
   <td style="text-align:left;"> 0.913 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Liechtenstein </td>
   <td style="text-align:left;"> 0.912 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> United Kingdom </td>
   <td style="text-align:left;"> 0.909 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Japan </td>
   <td style="text-align:left;"> 0.903 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> South Korea </td>
   <td style="text-align:left;"> 0.901 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Israel </td>
   <td style="text-align:left;"> 0.899 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Luxembourg </td>
   <td style="text-align:left;"> 0.898 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> France </td>
   <td style="text-align:left;"> 0.897 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Belgium </td>
   <td style="text-align:left;"> 0.896 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Finland </td>
   <td style="text-align:left;"> 0.895 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Austria </td>
   <td style="text-align:left;"> 0.893 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 28 </td>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Slovenia </td>
   <td style="text-align:left;"> 0.890 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 29 </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Italy </td>
   <td style="text-align:left;"> 0.887 </td>
   <td style="text-align:left;"> 0.006 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Spain </td>
   <td style="text-align:left;"> 0.884 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Czech Republic </td>
   <td style="text-align:left;"> 0.878 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 34 </td>
   <td style="text-align:left;"> 29 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Greece </td>
   <td style="text-align:left;"> 0.866 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 35 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Brunei </td>
   <td style="text-align:left;"> 0.865 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Estonia </td>
   <td style="text-align:left;"> 0.865 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 37 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Andorra </td>
   <td style="text-align:left;"> 0.858 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 38 </td>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Cyprus </td>
   <td style="text-align:left;"> 0.856 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Malta </td>
   <td style="text-align:left;"> 0.856 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 33 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Qatar </td>
   <td style="text-align:left;"> 0.856 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Poland </td>
   <td style="text-align:left;"> 0.855 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> 37 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Lithuania </td>
   <td style="text-align:left;"> 0.848 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 43 </td>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Chile </td>
   <td style="text-align:left;"> 0.847 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 38 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Saudi Arabia </td>
   <td style="text-align:left;"> 0.847 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Slovakia </td>
   <td style="text-align:left;"> 0.845 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Portugal </td>
   <td style="text-align:left;"> 0.843 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> United Arab Emirates </td>
   <td style="text-align:left;"> 0.840 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> 43 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Hungary </td>
   <td style="text-align:left;"> 0.836 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Latvia </td>
   <td style="text-align:left;"> 0.830 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Argentina </td>
   <td style="text-align:left;"> 0.827 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Croatia </td>
   <td style="text-align:left;"> 0.827 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Bahrain </td>
   <td style="text-align:left;"> 0.824 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 53 </td>
   <td style="text-align:left;"> 48 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Montenegro </td>
   <td style="text-align:left;"> 0.807 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Russia </td>
   <td style="text-align:left;"> 0.804 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Romania </td>
   <td style="text-align:left;"> 0.802 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Kuwait </td>
   <td style="text-align:left;"> 0.800 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Very High </td>
  </tr>
<tr>
<td style="text-align:left;"> 410 </td>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Belarus </td>
   <td style="text-align:left;"> 0.796 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 510 </td>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Oman </td>
   <td style="text-align:left;"> 0.796 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 62 </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Barbados </td>
   <td style="text-align:left;"> 0.795 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 71 </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Uruguay </td>
   <td style="text-align:left;"> 0.795 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 81 </td>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Bulgaria </td>
   <td style="text-align:left;"> 0.794 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 91 </td>
   <td style="text-align:left;"> 56 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Kazakhstan </td>
   <td style="text-align:left;"> 0.794 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Bahamas </td>
   <td style="text-align:left;"> 0.792 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 111 </td>
   <td style="text-align:left;"> 59 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Malaysia </td>
   <td style="text-align:left;"> 0.789 </td>
   <td style="text-align:left;"> 0.010 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 121 </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Palau </td>
   <td style="text-align:left;"> 0.788 </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 131 </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Panama </td>
   <td style="text-align:left;"> 0.788 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> 62 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Antigua and Barbuda </td>
   <td style="text-align:left;"> 0.786 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 151 </td>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Seychelles </td>
   <td style="text-align:left;"> 0.782 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 161 </td>
   <td style="text-align:left;"> 64 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Mauritius </td>
   <td style="text-align:left;"> 0.781 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 171 </td>
   <td style="text-align:left;"> 65 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Trinidad and Tobago </td>
   <td style="text-align:left;"> 0.780 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 181 </td>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Costa Rica </td>
   <td style="text-align:left;"> 0.776 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 191 </td>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Serbia </td>
   <td style="text-align:left;"> 0.776 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 201 </td>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Cuba </td>
   <td style="text-align:left;"> 0.775 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 211 </td>
   <td style="text-align:left;"> 69 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Iran </td>
   <td style="text-align:left;"> 0.774 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 221 </td>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Georgia </td>
   <td style="text-align:left;"> 0.769 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 231 </td>
   <td style="text-align:left;"> 71 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Turkey </td>
   <td style="text-align:left;"> 0.767 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 241 </td>
   <td style="text-align:left;"> 71 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Venezuela </td>
   <td style="text-align:left;"> 0.767 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 251 </td>
   <td style="text-align:left;"> 73 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Sri Lanka </td>
   <td style="text-align:left;"> 0.766 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 261 </td>
   <td style="text-align:left;"> 74 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Saint Kitts and Nevis </td>
   <td style="text-align:left;"> 0.765 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 271 </td>
   <td style="text-align:left;"> 75 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Albania </td>
   <td style="text-align:left;"> 0.764 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 281 </td>
   <td style="text-align:left;"> 76 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Lebanon </td>
   <td style="text-align:left;"> 0.763 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 291 </td>
   <td style="text-align:left;"> 77 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Mexico </td>
   <td style="text-align:left;"> 0.762 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 30 </td>
   <td style="text-align:left;"> 78 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Azerbaijan </td>
   <td style="text-align:left;"> 0.759 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 31 </td>
   <td style="text-align:left;"> 79 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Brazil </td>
   <td style="text-align:left;"> 0.754 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 341 </td>
   <td style="text-align:left;"> 79 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Grenada </td>
   <td style="text-align:left;"> 0.754 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 351 </td>
   <td style="text-align:left;"> 81 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Bosnia and Herzegovina </td>
   <td style="text-align:left;"> 0.750 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 361 </td>
   <td style="text-align:left;"> 82 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Macedonia </td>
   <td style="text-align:left;"> 0.748 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 371 </td>
   <td style="text-align:left;"> 83 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Algeria </td>
   <td style="text-align:left;"> 0.745 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 381 </td>
   <td style="text-align:left;"> 84 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Armenia </td>
   <td style="text-align:left;"> 0.743 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 391 </td>
   <td style="text-align:left;"> 84 </td>
   <td style="text-align:left;"> (3) </td>
   <td style="text-align:left;"> Ukraine </td>
   <td style="text-align:left;"> 0.743 </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 401 </td>
   <td style="text-align:left;"> 86 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Jordan </td>
   <td style="text-align:left;"> 0.741 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 411 </td>
   <td style="text-align:left;"> 87 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Peru </td>
   <td style="text-align:left;"> 0.740 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 421 </td>
   <td style="text-align:left;"> 87 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Thailand </td>
   <td style="text-align:left;"> 0.740 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 431 </td>
   <td style="text-align:left;"> 89 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Ecuador </td>
   <td style="text-align:left;"> 0.739 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 441 </td>
   <td style="text-align:left;"> 90 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> China </td>
   <td style="text-align:left;"> 0.738 </td>
   <td style="text-align:left;"> 0.010 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 451 </td>
   <td style="text-align:left;"> 91 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Fiji </td>
   <td style="text-align:left;"> 0.736 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 461 </td>
   <td style="text-align:left;"> 92 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Mongolia </td>
   <td style="text-align:left;"> 0.735 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 471 </td>
   <td style="text-align:left;"> 92 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Saint Lucia </td>
   <td style="text-align:left;"> 0.735 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 481 </td>
   <td style="text-align:left;"> 94 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Jamaica </td>
   <td style="text-align:left;"> 0.730 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 491 </td>
   <td style="text-align:left;"> 95 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Colombia </td>
   <td style="text-align:left;"> 0.727 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 501 </td>
   <td style="text-align:left;"> 96 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Dominica </td>
   <td style="text-align:left;"> 0.726 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 511 </td>
   <td style="text-align:left;"> 97 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Suriname </td>
   <td style="text-align:left;"> 0.725 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 521 </td>
   <td style="text-align:left;"> 97 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Tunisia </td>
   <td style="text-align:left;"> 0.725 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 531 </td>
   <td style="text-align:left;"> 99 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Dominican Republic </td>
   <td style="text-align:left;"> 0.722 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 541 </td>
   <td style="text-align:left;"> 99 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Saint Vincent and the Grenadines </td>
   <td style="text-align:left;"> 0.722 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 551 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Tonga </td>
   <td style="text-align:left;"> 0.721 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 57 </td>
   <td style="text-align:left;"> 102 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Libya </td>
   <td style="text-align:left;"> 0.716 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> 103 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Belize </td>
   <td style="text-align:left;"> 0.706 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 59 </td>
   <td style="text-align:left;"> 104 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Samoa </td>
   <td style="text-align:left;"> 0.704 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> 105 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Maldives </td>
   <td style="text-align:left;"> 0.701 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 61 </td>
   <td style="text-align:left;"> 105 </td>
   <td style="text-align:left;"> (3) </td>
   <td style="text-align:left;"> Uzbekistan </td>
   <td style="text-align:left;"> 0.701 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 412 </td>
   <td style="text-align:left;"> 107 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Moldova </td>
   <td style="text-align:left;"> 0.699 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 512 </td>
   <td style="text-align:left;"> 108 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Botswana </td>
   <td style="text-align:left;"> 0.698 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 63 </td>
   <td style="text-align:left;"> 109 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Gabon </td>
   <td style="text-align:left;"> 0.697 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 72 </td>
   <td style="text-align:left;"> 110 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Paraguay </td>
   <td style="text-align:left;"> 0.693 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 82 </td>
   <td style="text-align:left;"> 111 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Egypt </td>
   <td style="text-align:left;"> 0.691 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 92 </td>
   <td style="text-align:left;"> 111 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Turkmenistan </td>
   <td style="text-align:left;"> 0.691 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 102 </td>
   <td style="text-align:left;"> 113 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Indonesia </td>
   <td style="text-align:left;"> 0.689 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 112 </td>
   <td style="text-align:left;"> 114 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Palestine </td>
   <td style="text-align:left;"> 0.684 </td>
   <td style="text-align:left;"> 0.006 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 122 </td>
   <td style="text-align:left;"> 115 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Vietnam </td>
   <td style="text-align:left;"> 0.683 </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 132 </td>
   <td style="text-align:left;"> 116 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Philippines </td>
   <td style="text-align:left;"> 0.682 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 142 </td>
   <td style="text-align:left;"> 117 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> El Salvador </td>
   <td style="text-align:left;"> 0.680 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 152 </td>
   <td style="text-align:left;"> 118 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Bolivia </td>
   <td style="text-align:left;"> 0.674 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 162 </td>
   <td style="text-align:left;"> 119 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> South Africa </td>
   <td style="text-align:left;"> 0.666 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 172 </td>
   <td style="text-align:left;"> 120 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Kyrgyzstan </td>
   <td style="text-align:left;"> 0.664 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 182 </td>
   <td style="text-align:left;"> 121 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Iraq </td>
   <td style="text-align:left;"> 0.649 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 192 </td>
   <td style="text-align:left;"> 122 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Cape Verde </td>
   <td style="text-align:left;"> 0.648 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 202 </td>
   <td style="text-align:left;"> 123 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Morocco </td>
   <td style="text-align:left;"> 0.647 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 212 </td>
   <td style="text-align:left;"> 124 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Nicaragua </td>
   <td style="text-align:left;"> 0.645 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 222 </td>
   <td style="text-align:left;"> 125 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Guatemala </td>
   <td style="text-align:left;"> 0.640 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 232 </td>
   <td style="text-align:left;"> 125 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Namibia </td>
   <td style="text-align:left;"> 0.640 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 262 </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Guyana </td>
   <td style="text-align:left;"> 0.638 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 272 </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Micronesia </td>
   <td style="text-align:left;"> 0.638 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 282 </td>
   <td style="text-align:left;"> 129 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Tajikistan </td>
   <td style="text-align:left;"> 0.627 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 292 </td>
   <td style="text-align:left;"> 130 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Honduras </td>
   <td style="text-align:left;"> 0.625 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 301 </td>
   <td style="text-align:left;"> 131 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> India </td>
   <td style="text-align:left;"> 0.624 </td>
   <td style="text-align:left;"> 0.009 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 311 </td>
   <td style="text-align:left;"> 132 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Bhutan </td>
   <td style="text-align:left;"> 0.607 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 321 </td>
   <td style="text-align:left;"> 133 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Timor Leste </td>
   <td style="text-align:left;"> 0.605 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 331 </td>
   <td style="text-align:left;"> 134 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Vanuatu </td>
   <td style="text-align:left;"> 0.597 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 342 </td>
   <td style="text-align:left;"> 135 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Congo, Republic of the </td>
   <td style="text-align:left;"> 0.592 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 352 </td>
   <td style="text-align:left;"> 135 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Equatorial Guinea </td>
   <td style="text-align:left;"> 0.592 </td>
   <td style="text-align:left;"> 0.010 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 362 </td>
   <td style="text-align:left;"> 137 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Kiribati </td>
   <td style="text-align:left;"> 0.588 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 372 </td>
   <td style="text-align:left;"> 138 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Laos </td>
   <td style="text-align:left;"> 0.586 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 382 </td>
   <td style="text-align:left;"> 139 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Bangladesh </td>
   <td style="text-align:left;"> 0.579 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 392 </td>
   <td style="text-align:left;"> 139 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Ghana </td>
   <td style="text-align:left;"> 0.579 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 402 </td>
   <td style="text-align:left;"> 139 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Zambia </td>
   <td style="text-align:left;"> 0.579 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 413 </td>
   <td style="text-align:left;"> 142 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> São Tomé and Príncipe </td>
   <td style="text-align:left;"> 0.574 </td>
   <td style="text-align:left;"> 0.009 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 422 </td>
   <td style="text-align:left;"> 143 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Cambodia </td>
   <td style="text-align:left;"> 0.563 </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 432 </td>
   <td style="text-align:left;"> 144 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Nepal </td>
   <td style="text-align:left;"> 0.558 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 442 </td>
   <td style="text-align:left;"> 145 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Myanmar </td>
   <td style="text-align:left;"> 0.556 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 452 </td>
   <td style="text-align:left;"> 146 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Kenya </td>
   <td style="text-align:left;"> 0.555 </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 462 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Pakistan </td>
   <td style="text-align:left;"> 0.550 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Medium </td>
  </tr>
<tr>
<td style="text-align:left;"> 414 </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Swaziland </td>
   <td style="text-align:left;"> 0.541 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 513 </td>
   <td style="text-align:left;"> 149 </td>
   <td style="text-align:left;"> (4) </td>
   <td style="text-align:left;"> Syria </td>
   <td style="text-align:left;"> 0.536 </td>
   <td style="text-align:left;"> 0.017 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 64 </td>
   <td style="text-align:left;"> 150 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Angola </td>
   <td style="text-align:left;"> 0.533 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 73 </td>
   <td style="text-align:left;"> 151 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Tanzania </td>
   <td style="text-align:left;"> 0.531 </td>
   <td style="text-align:left;"> 0.012 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 83 </td>
   <td style="text-align:left;"> 152 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Nigeria </td>
   <td style="text-align:left;"> 0.527 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 93 </td>
   <td style="text-align:left;"> 153 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Cameroon </td>
   <td style="text-align:left;"> 0.518 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 103 </td>
   <td style="text-align:left;"> 154 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Papua New Guinea </td>
   <td style="text-align:left;"> 0.516 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 113 </td>
   <td style="text-align:left;"> 154 </td>
   <td style="text-align:left;"> (4) </td>
   <td style="text-align:left;"> Zimbabwe </td>
   <td style="text-align:left;"> 0.516 </td>
   <td style="text-align:left;"> 0.009 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 123 </td>
   <td style="text-align:left;"> 156 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Solomon Islands </td>
   <td style="text-align:left;"> 0.515 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 133 </td>
   <td style="text-align:left;"> 157 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Mauritania </td>
   <td style="text-align:left;"> 0.513 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 143 </td>
   <td style="text-align:left;"> 158 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Madagascar </td>
   <td style="text-align:left;"> 0.512 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 153 </td>
   <td style="text-align:left;"> 159 </td>
   <td style="text-align:left;"> (3) </td>
   <td style="text-align:left;"> Rwanda </td>
   <td style="text-align:left;"> 0.498 </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 163 </td>
   <td style="text-align:left;"> 160 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Comoros </td>
   <td style="text-align:left;"> 0.497 </td>
   <td style="text-align:left;"> 0.001 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 173 </td>
   <td style="text-align:left;"> 160 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Lesotho </td>
   <td style="text-align:left;"> 0.497 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 183 </td>
   <td style="text-align:left;"> 162 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Senegal </td>
   <td style="text-align:left;"> 0.494 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 193 </td>
   <td style="text-align:left;"> 163 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Haiti </td>
   <td style="text-align:left;"> 0.493 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 203 </td>
   <td style="text-align:left;"> 163 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Uganda </td>
   <td style="text-align:left;"> 0.493 </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 213 </td>
   <td style="text-align:left;"> 165 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Sudan </td>
   <td style="text-align:left;"> 0.490 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 223 </td>
   <td style="text-align:left;"> 166 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Togo </td>
   <td style="text-align:left;"> 0.487 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 233 </td>
   <td style="text-align:left;"> 167 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Benin </td>
   <td style="text-align:left;"> 0.485 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 242 </td>
   <td style="text-align:left;"> 168 </td>
   <td style="text-align:left;"> (9) </td>
   <td style="text-align:left;"> Yemen </td>
   <td style="text-align:left;"> 0.482 </td>
   <td style="text-align:left;"> 0.017 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 273 </td>
   <td style="text-align:left;"> 169 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Afghanistan </td>
   <td style="text-align:left;"> 0.479 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 283 </td>
   <td style="text-align:left;"> 170 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Malawi </td>
   <td style="text-align:left;"> 0.476 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 293 </td>
   <td style="text-align:left;"> 171 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Côte d'Ivoire </td>
   <td style="text-align:left;"> 0.474 </td>
   <td style="text-align:left;"> 0.008 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 302 </td>
   <td style="text-align:left;"> 172 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Djibouti </td>
   <td style="text-align:left;"> 0.473 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 312 </td>
   <td style="text-align:left;"> 173 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Gambia </td>
   <td style="text-align:left;"> 0.452 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 322 </td>
   <td style="text-align:left;"> 174 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Ethiopia </td>
   <td style="text-align:left;"> 0.448 </td>
   <td style="text-align:left;"> 0.007 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 332 </td>
   <td style="text-align:left;"> 175 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Mali </td>
   <td style="text-align:left;"> 0.442 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 343 </td>
   <td style="text-align:left;"> 176 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Congo, Democratic Republic of the </td>
   <td style="text-align:left;"> 0.435 </td>
   <td style="text-align:left;"> 0.010 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 353 </td>
   <td style="text-align:left;"> 177 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Liberia </td>
   <td style="text-align:left;"> 0.427 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 363 </td>
   <td style="text-align:left;"> 178 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Guinea Bissau </td>
   <td style="text-align:left;"> 0.424 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 373 </td>
   <td style="text-align:left;"> 179 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> Eritrea </td>
   <td style="text-align:left;"> 0.420 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 383 </td>
   <td style="text-align:left;"> 179 </td>
   <td style="text-align:left;"> (3) </td>
   <td style="text-align:left;"> Sierra Leone </td>
   <td style="text-align:left;"> 0.420 </td>
   <td style="text-align:left;"> 0.011 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 393 </td>
   <td style="text-align:left;"> 181 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Mozambique </td>
   <td style="text-align:left;"> 0.418 </td>
   <td style="text-align:left;"> 0.004 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 403 </td>
   <td style="text-align:left;"> 181 </td>
   <td style="text-align:left;"> (2) </td>
   <td style="text-align:left;"> South Sudan </td>
   <td style="text-align:left;"> 0.418 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 415 </td>
   <td style="text-align:left;"> 183 </td>
   <td style="text-align:left;"> (1) </td>
   <td style="text-align:left;"> Guinea </td>
   <td style="text-align:left;"> 0.414 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 423 </td>
   <td style="text-align:left;"> 184 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Burundi </td>
   <td style="text-align:left;"> 0.404 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 433 </td>
   <td style="text-align:left;"> 185 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Burkina Faso </td>
   <td style="text-align:left;"> 0.402 </td>
   <td style="text-align:left;"> 0.003 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 443 </td>
   <td style="text-align:left;"> 186 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Chad </td>
   <td style="text-align:left;"> 0.396 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 453 </td>
   <td style="text-align:left;"> 187 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Niger </td>
   <td style="text-align:left;"> 0.353 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 463 </td>
   <td style="text-align:left;"> 188 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Central African Republic </td>
   <td style="text-align:left;"> 0.352 </td>
   <td style="text-align:left;"> 0.005 </td>
   <td style="text-align:left;"> Low </td>
  </tr>
</tbody>
</table>

```r
dim(hdicomb)
```

```
## [1] 188   6
```

```r
#3c
#write table to csv(6a)
#setwd("C:/Users/jjschued/Documents")
write.table(hdicomb, file = "hdicomb.csv", col.names = TRUE, row.names = FALSE, sep =",")

#merge procrastination and and hdicomb
colnames(hdicomb)[3] <- "Countryres"
procrastcomb <- merge(procrast, hdicomb, by=c("Countryres"), all = FALSE )



kable(head(procrastcomb), "html", padding = 2, longtable = TRUE, row.names = TRUE) %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<thead><tr>
<th style="text-align:left;">   </th>
   <th style="text-align:left;"> Countryres </th>
   <th style="text-align:right;"> Age </th>
   <th style="text-align:left;"> Gender </th>
   <th style="text-align:left;"> Kids </th>
   <th style="text-align:left;"> Edu </th>
   <th style="text-align:left;"> WorkStatus </th>
   <th style="text-align:right;"> AnnualIncome </th>
   <th style="text-align:left;"> CurrentOccup </th>
   <th style="text-align:right;"> Timeinposyr </th>
   <th style="text-align:right;"> Timeinposmn </th>
   <th style="text-align:left;"> Communitysize </th>
   <th style="text-align:left;"> MaritalStatus </th>
   <th style="text-align:left;"> Numsons </th>
   <th style="text-align:right;"> Numdaughters </th>
   <th style="text-align:right;"> DP1 </th>
   <th style="text-align:right;"> DP2 </th>
   <th style="text-align:right;"> DP3 </th>
   <th style="text-align:right;"> DP4 </th>
   <th style="text-align:right;"> DP5 </th>
   <th style="text-align:right;"> AIP1 </th>
   <th style="text-align:right;"> AIP2 </th>
   <th style="text-align:right;"> AIP3 </th>
   <th style="text-align:right;"> AIP4 </th>
   <th style="text-align:right;"> AIP5 </th>
   <th style="text-align:right;"> AIP6 </th>
   <th style="text-align:right;"> AIP7 </th>
   <th style="text-align:right;"> AIP8 </th>
   <th style="text-align:right;"> AIP9 </th>
   <th style="text-align:right;"> AIP10 </th>
   <th style="text-align:right;"> AIP11 </th>
   <th style="text-align:right;"> AIP12 </th>
   <th style="text-align:right;"> AIP13 </th>
   <th style="text-align:right;"> AIP14 </th>
   <th style="text-align:right;"> AIP15 </th>
   <th style="text-align:right;"> GP1 </th>
   <th style="text-align:right;"> GP2 </th>
   <th style="text-align:right;"> GP3 </th>
   <th style="text-align:right;"> GP4 </th>
   <th style="text-align:right;"> GP5 </th>
   <th style="text-align:right;"> GP6 </th>
   <th style="text-align:right;"> GP7 </th>
   <th style="text-align:right;"> GP8 </th>
   <th style="text-align:right;"> GP9 </th>
   <th style="text-align:right;"> GP10 </th>
   <th style="text-align:right;"> GP11 </th>
   <th style="text-align:right;"> GP12 </th>
   <th style="text-align:right;"> GP13 </th>
   <th style="text-align:right;"> GP14 </th>
   <th style="text-align:right;"> GP15 </th>
   <th style="text-align:right;"> GP16 </th>
   <th style="text-align:right;"> GP17 </th>
   <th style="text-align:right;"> GP18 </th>
   <th style="text-align:right;"> GP19 </th>
   <th style="text-align:right;"> GP20 </th>
   <th style="text-align:right;"> SWLS1 </th>
   <th style="text-align:right;"> SWLS2 </th>
   <th style="text-align:right;"> SWLS3 </th>
   <th style="text-align:right;"> SWLS4 </th>
   <th style="text-align:right;"> SWLS5 </th>
   <th style="text-align:left;"> UCONSPRO </th>
   <th style="text-align:left;"> OTHCONSPRO </th>
   <th style="text-align:right;"> dpmean </th>
   <th style="text-align:right;"> aipmean </th>
   <th style="text-align:right;"> gpmean </th>
   <th style="text-align:right;"> swlsmean </th>
   <th style="text-align:left;"> Rank </th>
   <th style="text-align:left;"> RankChng </th>
   <th style="text-align:left;"> HDI </th>
   <th style="text-align:left;"> HDICHNG </th>
   <th style="text-align:left;"> HDICAT </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Afghanistan </td>
   <td style="text-align:right;"> 55.0 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> Yes Kids </td>
   <td style="text-align:left;"> ma </td>
   <td style="text-align:left;"> full-time </td>
   <td style="text-align:right;"> 150000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Large Town </td>
   <td style="text-align:left;"> Married </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:right;"> 3.6 </td>
   <td style="text-align:right;"> 3.866667 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:left;"> 169 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.479 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Afghanistan </td>
   <td style="text-align:right;"> 80.0 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> No Kids </td>
   <td style="text-align:left;"> grade </td>
   <td style="text-align:left;"> unemployed </td>
   <td style="text-align:right;"> 10000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Medium-Sized </td>
   <td style="text-align:left;"> Single </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 2.733333 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:left;"> 169 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.479 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Afghanistan </td>
   <td style="text-align:right;"> 55.0 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> No Kids </td>
   <td style="text-align:left;"> deg </td>
   <td style="text-align:left;"> full-time </td>
   <td style="text-align:right;"> 87500 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> Large Town </td>
   <td style="text-align:left;"> Married </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 2.666667 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:left;"> 169 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.479 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Afghanistan </td>
   <td style="text-align:right;"> 55.0 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> Yes Kids </td>
   <td style="text-align:left;"> deg </td>
   <td style="text-align:left;"> full-time </td>
   <td style="text-align:right;"> 10000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Village </td>
   <td style="text-align:left;"> Divorced </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 3.066667 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:left;"> 169 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.479 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Low </td>
  </tr>
<tr>
<td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Albania </td>
   <td style="text-align:right;"> 45.0 </td>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:left;"> No Kids </td>
   <td style="text-align:left;"> ltuni </td>
   <td style="text-align:left;"> full-time </td>
   <td style="text-align:right;"> 150000 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Large Town </td>
   <td style="text-align:left;"> Single </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 2.066667 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:left;"> 75 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.764 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
<tr>
<td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Albania </td>
   <td style="text-align:right;"> 67.5 </td>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:left;"> Yes Kids </td>
   <td style="text-align:left;"> deg </td>
   <td style="text-align:left;"> part-time </td>
   <td style="text-align:right;"> 87500 </td>
   <td style="text-align:left;"> MEDIA CONSULTANT </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> Large-City </td>
   <td style="text-align:left;"> Divorced </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> yes </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 2.400000 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:left;"> 75 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 0.764 </td>
   <td style="text-align:left;"> 0.002 </td>
   <td style="text-align:left;"> High </td>
  </tr>
</tbody>
</table>

```r
#4a remove everone under 18 and over 65 since this is for a work analysis since people typically stop working at 65

procrastcomb <- procrastcomb[procrastcomb$Age > 18, ]
procrastcomb <- procrastcomb[procrastcomb$Age < 65, ]

#convert HDI to numeric

procrastcomb$HDI <- as.numeric(format(procrastcomb$HDI, scientific = F))
```

```
## Warning: NAs introduced by coercion
```

```r
#write table to csv(6b)
write.table(procrastcomb, file = "procrastcomb.csv", col.names = TRUE, row.names = FALSE, sep =",")
```



Summary statistics of the important parameters are presented below for all of the employee survey responses.  This gives a baseline for what an employer would expect on average for an employee across the world.  Following the summary statistics there are several visuals that help represent this information.  It shows that motivation or procrastination levels are an evenly distributed value with most people falling in the middle.  Income, on the other hand is left skewed with a few clusters of higher income employees, but many others on the lower end of the pay spectrum.  Following the visualizations, some additional summary information is presented about the respondents.  These include: gender, employment status,occupation titles, country of residence, and procrastination perception.  The gender count is evenly split, while most workers are full time.  Occupation title was a very inconsistent field therefore there is a concentration of frequent titles at the top and quickly trailing off to many additional titles with only one respondent. This is sorted in descending order to easily identify top titles.  Next, Country of residence is displayed in descending order as well and it can be noted many respondents are from the United States. Finally, an analysis of perceptions are presented.  Shown are comparisons of where perceptions match and where they do not match between the respondents and others. This shows that most people consider themselves procrastinators.


```r
# 4b summary statistics of procrastcomb
sumtable <- summary(procrastcomb$Age)
Statistics <- c("Min.", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max.", "NA's" )
Age <- c(sumtable[1], sumtable[2],  sumtable[3], sumtable[4], sumtable[5], sumtable[6], sumtable[7] )

sumtable <- summary(procrastcomb$AnnualIncome)
Income <- c(sumtable[1], sumtable[2],  sumtable[3], sumtable[4], sumtable[5], sumtable[6], sumtable[7] )

sumtable <- summary(procrastcomb$HDI)
HDI <- c(sumtable[1], sumtable[2],  sumtable[3], sumtable[4], sumtable[5], sumtable[6], sumtable[7] )
sumtable <- summary(procrastcomb$aipmean)
AIPmean <- c(sumtable[1], sumtable[2],  sumtable[3], sumtable[4], sumtable[5], sumtable[6], sumtable[7] )
sumtable <- summary(procrastcomb$dpmean)
DPmean <- c(sumtable[1], sumtable[2],  sumtable[3], sumtable[4], sumtable[5], sumtable[6], sumtable[7] )
sumtable <- summary(procrastcomb$gpmean)
GPmean <- c(sumtable[1], sumtable[2],  sumtable[3], sumtable[4], sumtable[5], sumtable[6], sumtable[7] )
sumtable <- summary(procrastcomb$swlsmean)
SSWLSmean <- c(sumtable[1], sumtable[2],  sumtable[3], sumtable[4], sumtable[5], sumtable[6], sumtable[7] )
sumdf <- data.frame(Statistics, Age, Income, HDI, AIPmean, DPmean, GPmean, SSWLSmean)
kable(sumdf, "html", padding = 2, longtable = TRUE, row.names = FALSE) %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<thead><tr>
<th style="text-align:left;"> Statistics </th>
   <th style="text-align:right;"> Age </th>
   <th style="text-align:right;"> Income </th>
   <th style="text-align:right;"> HDI </th>
   <th style="text-align:right;"> AIPmean </th>
   <th style="text-align:right;"> DPmean </th>
   <th style="text-align:right;"> GPmean </th>
   <th style="text-align:right;"> SSWLSmean </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;"> Min. </td>
   <td style="text-align:right;"> 19.00000 </td>
   <td style="text-align:right;"> 10000.00 </td>
   <td style="text-align:right;"> 0.4790000 </td>
   <td style="text-align:right;"> 1.000000 </td>
   <td style="text-align:right;"> 1.000000 </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> 1.000000 </td>
  </tr>
<tr>
<td style="text-align:left;"> 1st Qu. </td>
   <td style="text-align:right;"> 28.00000 </td>
   <td style="text-align:right;"> 15000.00 </td>
   <td style="text-align:right;"> 0.9200000 </td>
   <td style="text-align:right;"> 2.400000 </td>
   <td style="text-align:right;"> 2.400000 </td>
   <td style="text-align:right;"> 2.8000 </td>
   <td style="text-align:right;"> 2.400000 </td>
  </tr>
<tr>
<td style="text-align:left;"> Median </td>
   <td style="text-align:right;"> 32.50000 </td>
   <td style="text-align:right;"> 45000.00 </td>
   <td style="text-align:right;"> 0.9200000 </td>
   <td style="text-align:right;"> 3.000000 </td>
   <td style="text-align:right;"> 3.000000 </td>
   <td style="text-align:right;"> 3.3000 </td>
   <td style="text-align:right;"> 3.000000 </td>
  </tr>
<tr>
<td style="text-align:left;"> Mean </td>
   <td style="text-align:right;"> 36.42086 </td>
   <td style="text-align:right;"> 58449.64 </td>
   <td style="text-align:right;"> 0.9054552 </td>
   <td style="text-align:right;"> 2.987696 </td>
   <td style="text-align:right;"> 3.058052 </td>
   <td style="text-align:right;"> 3.2614 </td>
   <td style="text-align:right;"> 3.030769 </td>
  </tr>
<tr>
<td style="text-align:left;"> 3rd Qu. </td>
   <td style="text-align:right;"> 45.00000 </td>
   <td style="text-align:right;"> 67500.00 </td>
   <td style="text-align:right;"> 0.9200000 </td>
   <td style="text-align:right;"> 3.600000 </td>
   <td style="text-align:right;"> 3.800000 </td>
   <td style="text-align:right;"> 3.7500 </td>
   <td style="text-align:right;"> 3.800000 </td>
  </tr>
<tr>
<td style="text-align:left;"> Max. </td>
   <td style="text-align:right;"> 55.00000 </td>
   <td style="text-align:right;"> 250000.00 </td>
   <td style="text-align:right;"> 0.9490000 </td>
   <td style="text-align:right;"> 5.000000 </td>
   <td style="text-align:right;"> 5.000000 </td>
   <td style="text-align:right;"> 5.0000 </td>
   <td style="text-align:right;"> 5.000000 </td>
  </tr>
<tr>
<td style="text-align:left;"> NA's </td>
   <td style="text-align:right;"> 6.00000 </td>
   <td style="text-align:right;"> 324.00 </td>
   <td style="text-align:right;"> 6.0000000 </td>
   <td style="text-align:right;"> 6.000000 </td>
   <td style="text-align:right;"> 6.000000 </td>
   <td style="text-align:right;"> 6.0000 </td>
   <td style="text-align:right;"> 6.000000 </td>
  </tr>
</tbody>
</table>

```r
#Histograms of two vairables above.  AIP is very much a bell shape curve and evenly distributed, while income is a left skewed curve with most falling on the lower end of the income scale
hist(procrastcomb$aipmean, col="Green", ylab ="AIP Mean", xlab
="FREQUENCY", main = "AIP Mean Histogram")
```

![](CaseStudy26306_files/figure-html/prelimanalysi-1.png)<!-- -->

```r
hist(procrastcomb$AnnualIncome, col="Blue", ylab ="Annual Income", xlab
="FREQUENCY", main = "Annual Income Histogram")
```

![](CaseStudy26306_files/figure-html/prelimanalysi-2.png)<!-- -->

```r
## 4c frequencies of Gender, work Status, Occupation
Gendersum <- count(procrastcomb, 'Gender')
WStatussm <- count(procrastcomb, 'WorkStatus')
OccuStSm <- count(procrastcomb, 'CurrentOccup')

OccuStSm <- OccuStSm[order(-OccuStSm$freq),]

kable(Gendersum, "html", padding = 2, longtable = TRUE, row.names = FALSE) %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<thead><tr>
<th style="text-align:left;"> Gender </th>
   <th style="text-align:right;"> freq </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;">  </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 2088 </td>
  </tr>
<tr>
<td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 1521 </td>
  </tr>
<tr>
<td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
</tbody>
</table>

```r
kable(WStatussm, "html", padding = 2, longtable = TRUE, row.names = FALSE) %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<thead><tr>
<th style="text-align:left;"> WorkStatus </th>
   <th style="text-align:right;"> freq </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;">  </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
<tr>
<td style="text-align:left;"> full-time </td>
   <td style="text-align:right;"> 2115 </td>
  </tr>
<tr>
<td style="text-align:left;"> part-time </td>
   <td style="text-align:right;"> 413 </td>
  </tr>
<tr>
<td style="text-align:left;"> retired </td>
   <td style="text-align:right;"> 56 </td>
  </tr>
<tr>
<td style="text-align:left;"> student </td>
   <td style="text-align:right;"> 777 </td>
  </tr>
<tr>
<td style="text-align:left;"> unemployed </td>
   <td style="text-align:right;"> 232 </td>
  </tr>
<tr>
<td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
</tbody>
</table>

```r
kable(OccuStSm, "html", padding = 2, longtable = TRUE, row.names = FALSE) %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<thead><tr>
<th style="text-align:left;"> CurrentOccup </th>
   <th style="text-align:right;"> freq </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;">  </td>
   <td style="text-align:right;"> 1825 </td>
  </tr>
<tr>
<td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 526 </td>
  </tr>
<tr>
<td style="text-align:left;"> TECHNICAL </td>
   <td style="text-align:right;"> 86 </td>
  </tr>
<tr>
<td style="text-align:left;"> TEACHER </td>
   <td style="text-align:right;"> 71 </td>
  </tr>
<tr>
<td style="text-align:left;"> ATTORNEY </td>
   <td style="text-align:right;"> 46 </td>
  </tr>
<tr>
<td style="text-align:left;"> COLLEGE PROFESSOR </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
<tr>
<td style="text-align:left;"> MANAGER </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
<tr>
<td style="text-align:left;"> RESEARCH </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENGINEER </td>
   <td style="text-align:right;"> 29 </td>
  </tr>
<tr>
<td style="text-align:left;"> EDITOR </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
<tr>
<td style="text-align:left;"> FINANCIAL </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
<tr>
<td style="text-align:left;"> MARKETING </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
<tr>
<td style="text-align:left;"> DIRECTOR </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERER </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
<tr>
<td style="text-align:left;"> UNEMPLOYED </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
<tr>
<td style="text-align:left;"> HOUSWIFE </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSISTANT PROFESSOR </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
<tr>
<td style="text-align:left;"> DOCTOR; PHYSICIAN </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
<tr>
<td style="text-align:left;"> NURSE </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
<tr>
<td style="text-align:left;"> SALES </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
<tr>
<td style="text-align:left;"> CONSULTANT </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
<tr>
<td style="text-align:left;"> SCIENTIST </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
<tr>
<td style="text-align:left;"> STUDENT </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
<tr>
<td style="text-align:left;"> HOME MAKER </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
<tr>
<td style="text-align:left;"> ADMINISTRATOR </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
<tr>
<td style="text-align:left;"> GRAPHIC DESIGNER </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROJECT MANAGER </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
<tr>
<td style="text-align:left;"> SELF EMPLOYED </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
<tr>
<td style="text-align:left;"> SERVER </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
<tr>
<td style="text-align:left;"> CUSTOMER SERVICE </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
<tr>
<td style="text-align:left;"> INSURANCE </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
<tr>
<td style="text-align:left;"> LEGAL </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
<tr>
<td style="text-align:left;"> LIBRARIAN </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
<tr>
<td style="text-align:left;"> RETIRED </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUSINESS OWNER </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> MEDICAL </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRESIDENT </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> PSYCHOLOGIS </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> REAL ESTATE </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> SOCIAL WORKER </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> TRANSLATOR </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> ANALYST </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> ARTIST </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> CLERK </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> GRADUATE ASSISTANT </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> JOURNALIST </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> MARKET ANALYST </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> OFFICE </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> PASTOR ; LIFE COACH  CLERGY </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> ADMINISTRATION ASSISTANT </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> ARCHITECT </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> NANNY </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> OPERATIONS MANAGER </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> RETAIL </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> RN </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> SECRETARY </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> SPECIAL EDUCATION TEACHER </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> TEACHER </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> TUTOR </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> WEB DESIGNER </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> BANK TELLER </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> CEO </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> DEPUTY DIRECTOR </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> GRADUATE RESEARCH </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> HUMAN RESOURCE MANAGER </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> MUSICIAN </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNTANT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> ADMIN ASSIST </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> COMMUNICATIONS </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> DESIGNER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> DIPLOMAT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> EPIDEMIOLOGIST </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> VETERINARIAN </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACADEMIC ASSISTANT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNTING </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNTING MANAGER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> ART DIRECTOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSOCIATE </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSOCIATE DIRECTOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> BANKER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> CHIEF OF STAFF </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> CIVIL SERVANT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> CLUTTER CLEARER,  VIDEO EDITOR, CATERER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> CONSUMER CASE COORDINATOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> COPY WRITERER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> COUNSELOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> CREATIVE DIRECTOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> DENTIST </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> DESIGNER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> DRIVER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> EXECUTIVE ASSISTANT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> EXECUTIVE DIRECTOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> GEOLOGIST </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> GRADUATESTUDENT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> INTERNSHIP </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> IT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> JOURNALIST (FREELANCE) </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> LETTER CARRIER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> LIBRARY TECHNICAL </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> MARKET RESEARCH </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> MARKETING COPYWRITERER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> NETWORK ENGINEER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> OWNER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> PARALEGAL </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> PARAPROFESSIONAL </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> PHARMACIST </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> POLICY ANALYST </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> POSTDOC </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> PSYCHOTHERAPIST </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> RECEPTIONIST </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> STOCKER </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> SUPERVISOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> TRAINING COORDINATOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> VICE-PRESIDENT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERER/EDITOR </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERING CONSULTANT </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACUPUNCTURIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ATTORNEY-SELF EMPLOYED </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BOOKSELLER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INNKEEPER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MILITARY </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RESEARCH </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> UNIVERSITY FACULTY </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> 'UTTERLY SHIFTLESS ARTSSTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ABC </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACADEMIC </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNT MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNT PLANNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNT SERVICE REP </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNTING ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNTS PAYABLE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACCOUNTS PAYABLE / FLEET MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACOUNTING ANALYST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACTIVITIES LEADER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ACTRESS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ADMINISTRATIVE ASISTANT FOR JEWELRY STOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ADMINISTRATIVE OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ADVOCATE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> AGRONOMIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> AIRLINE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> AIRPORT GROUND HANDLER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ANTIQUE DEALER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ARTIST/ DESIGNER/BUILDER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ARTIST/ADMINISTRATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ARTIST/DESIGNER/HOMEMAKER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSISTANT DISTRICT ATTORNEY </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSISTANT GENERAL COUNSEL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSOC. GOVERNMENTAL PROGRAM ANALYST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSOCIATE / INVESTMENT BANKING </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSOCIATE AT LEGAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASSOCIATE PRODUCER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASST. PRE-SCHOOL TEACHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ASTROHYSICIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ATTORNEY - SELF EMPLOYED </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ATTORNEY – ASSOCIATE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> AUDIO ENGINEER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> AVIATION SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BAR &amp; RESTAURANT OWNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BARTENDER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BIOLOGIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BOOKKEEPER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BOOKKEEPER/ ACTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BOX OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BRAILLIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUDGET ANALYST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUSINESS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUSINESS / TEST ANALYST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUSINESS CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUSINESS MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUSINESS TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUSINESSWOMAN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> BUYER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> C E O/ M D </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CAD OPERATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CAD TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CAMERA COORDINATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CAMPUS PLANNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CAPSTONE GOLF COURSE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CAREER PLACEMENT ASSOCIATE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CASE MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CASTING DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CATALOGUER /  FREELANCE ARTIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CATHOLIC PRIEST/ FULL TIMSTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CERTIFIED NURSE'S ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CHAIRMAN OF THE BOARD </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CHIEF FINANCIAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CHIEFE DEVELOPMENT ENGINEER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CHIROPRACTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CLIENT RELATIONSHIP ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CLINICAL DIETITIAN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CLINICAL PSYCHOLOGIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CLINICAL RESEARCH </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CLINICAL TRIAL ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CO-PROPRIETOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COLLECTION MANAGEMENT SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COLLEGE ADMINISTRATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COLLEGE FACULTY </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COMMUNICATIONS &amp; PUBLISHING </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COMPANY DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CONSULTANT AND ENTREPRENEUR (SMALL BUSIN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CONSULTING MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CONTROLLER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CONTSURUCTION MANAGEMENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COORDINATOR OF INTERNATIONAL PROGRAMS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COORDINATORE OPERATIVO </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COPY SUPERVISOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CORPORATE INSTRUCTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CORPORATE TRAINER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CORPORATION PRESIDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CORRECTIONS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> COUNTRY STYLE EMPLOYEE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CREATIVE CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> CRNA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DANCE TEACHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DATA WAREHOUSE ENGINEER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DEALER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DENTAL &amp; DISABILITY COORDINATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DEPT. DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DEPUTY CHIEF OF PUBLIC TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DEPUTY CHIEIF TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DEPUTY PRACTICE MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DETAIL CHECKER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DISABILITY ALLOWANCE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DISH WASHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DIVISIONAL MANAGER OF A LARGE COSMETICS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> DOCTOR RESEARCH </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EARLY CHILD HOOD TEACHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EARLY CHILDHOOD EDUCATIONSTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ECOLOGY TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ECONOMIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ECONOMY </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EDITOR ATTORNEY </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EDUCATION </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EDUCATION (AT A UNIVERSITY) </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EDUCATION ADMINISTRATION </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EDUCATION SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EDUCATORSTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EFL TEACHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EHS MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ELECTION SERVICES </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ELECTRICAL TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ELECTRONIC TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EMPLOYED BY A CHURCH </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EMT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENERGY THERAPIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENOLOGIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENTERTAINER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENTREPRENEUR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENTREPRENEUR &amp; CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENVIRONMENTAL ANALYST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENVIRONMENTAL ENGINEER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ENVIRONMENTAL SENIOR SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EOD </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ESL TEACHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EXECUTIVE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EXECUTIVE OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> EXECUTIVE VICE PRESIDENT / SENIOR LENDER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FACILITATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FACILITIES MANAGEMENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FEDERAL EXCISE TAX AUDITOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FIELD COORDINATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FILM EDITOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FILM INDUSTRY/MISCELANIOUS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FILM MAKER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FIRST VP &amp; ASSOCIATE GENERAL COUNSEL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FITNESS ASSISTANT / WELLNESS MENTOR / CA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FITNESS INSTRUCTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FLIGHT SURGEON </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FOOD SERVICE SUPERVISOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FOREIGN AFFAIRS SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FRAMER/SALES </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FREE LANCE BOOKKEEPER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FREELANCE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FREELANCE ESL TEACHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FREELANCE MUSICIAN / PART TIME EMT / PRI </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FREELANCE PROJECT MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FULL TIMESTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FULL-TIME MOTHER / PART-TIME EDITOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FULLTIME OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> FURNITURE MAKER, HOME RESTORER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> GENDER/PUBLIC HEALTH CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> GEOPHYSICIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> GRANTS ADMINISTRATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> GROUNDSKEEPER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HEAD - OPERATIONS &amp; QA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HEALTH CARE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HEALTHCARE CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HOST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HOSTESS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HOTEL DESK CLERK </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HOUSEKEEPING </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HR GENERALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HUMAN RESOURCE MANGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> HVAC TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ICT DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> IN-HOUSE LEGAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INSTRUCTIONAL ASSISTANT ONLINE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INSTRUCTOR / COACH </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INTERN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INTERNET &amp; MEDIA CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INTERPRETER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INVESTIGATIVE SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INVESTMENT ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> INVESTMENT BANKER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ISTRAINING COORDINATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> JANITOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> JEWELRY ARTIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> JUVENILE CORRECTIONS OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LAB DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LAB SERVICES ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LABOR RELATIONS SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LABORATORY TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LABORER (CONSTRUCTION) </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LAND USE PLANNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LANDSCAPE DESIGNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LANGUAGE SERVICE PROVIDER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LANGUAGE TRAINER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LECTURER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LEGISLATION ANALYST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LIBRARY ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LIBRARY PARAPROFESSIONAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LICENSED PROFESSIONAL COUNSELOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LIFE GUARD </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> LPN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MAINTENANCE TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MANAGEMENT CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MANAGEMENT CONSULTANT &amp; ENTREPRENEUR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MANAGER - ANALYTICAL AND ENVIRONMENTAL S </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MANAGER IT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MANAGER,INTERACITVE MEDIA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MANUFACTURING </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MASSAGE THERAPIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MASTER CONTROL OPERATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MD </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MECHANICAL ENGINEER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MEDIA RELATIONS MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MEDIA RELATIONS/SCIENCE WRITERING </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MENTOR/SPECIAL EVENTS INTERN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MKTG </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MOVER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MULTIMEDIA DEVELOPER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MUSEUM DOCENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> MUSICIANSTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> NANNY ANDSTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> NETWORK SERVICES ENGINEER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> NEW REAL ESTATE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> NEWSPAPER CARRIER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> NIGHT DISPATCH SUPERVISOR  (IT'S JUST A </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> NON-PROFTECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> NURSING HOME </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ONLINE MEDIA BUYER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ORGANIC GROCERY STORE CASHIER/SHIFT LEAD </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ORNITHOLOGY GRADUATESTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> OUTDOOR RECREATION COORDINATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> OWNER - PRIVATE PRACTICE PHYSICAL THERAP </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> P-T COLLEGE FACULTY &amp; P-T SELF EMPLOYED </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PAGE DESIGNER FOR A NEWSPAPER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PARENT EDUCATOR/SUPERVISOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PARTNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PATHOLOGY </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PCA FOR A QUADRAPILEGIC AND A PCA FOR A </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PHARMACEUTICAL MERCHANDISER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PHARMACY TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PHDSTUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PHYSICAL SCIENCE TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PHYSICIAN (INTERNIST) </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PHYSICIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PHYSIOTHERAPST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PJUBLIC RELATIONS DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PLANT ENGINEERING SUPERVISOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> POST GRAD PHYSICIAN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> POSTDOCTORAL RESEARCH </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PR AND COMMUNICATIONS FIRM OWNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRESIDENT/CEO </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRESS OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRIVATE EQUITY PRINCIPAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRO POKER PLAYER /   WEBSITE OWNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROBATION SUPERVISOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROCESS ENGINEER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRODUCE ASSOCIATE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRODUCER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRODUCT FIELD TEST MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PRODUCTION OPERATIONS SUPPORT ANALYST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROFESSIONAL ORGANIZER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROGRAM ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROGRAM COORDINATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROGRAM DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROGRAM MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROGRAM MANAGER AND ACTING DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROGRAM OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROGRAM SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROOFREADER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PROPOSAL DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PUBLIC HEALTH </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PUBLIC RELATIONS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> PUBLISHING </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> QUALITY MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> QUOTATIONS SPECIALIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> REASEARCH ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RECREATIONAL STAFF </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> REGIONAL SALES </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> REGULATORY AFFAIRS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RESIDENCE DON </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RESIDENT PHYSICIAN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RESIDENTIAL SERVICES SUPERVISOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RESPIRATORY THERAPIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RESTAURANT MGR /STUDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RESTAURANT OPERATIONS MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RETAIL / ARTIST /WRITERER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RETIRED/ADJUNCT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> RN - MEDICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> ROCKET SCIENTIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SCHOOL COUNSELOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SCIENCE WRITERING INTERN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SENIOR CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SENIOR CONSULTANT TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SENIOR GRANT OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SENIOR HUMAN RESOURCES CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SENIOR POLICY ADVISOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SENIOR PROJECT MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SENIOR STAFF WRITERER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SENIOR TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SERVICE CO-ORDINATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SERVICE REGISTRAR/ENGLISH INSTRUCTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SET DESIGNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SET LIGHTING TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SHIPPING/RECEIVING/WAREHOUSE MGNT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SOCIAL MEDIA CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SOCIAL POLICY ANALYST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SOCIAL WORK INTERN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SPEAKER AUTHOR CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SPEAKER/ACTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SPECIAL EDUCATION ADMINISTRATIVE ASSISTA </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SPECIAL PROJECTS EDITOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SPEECH AND LANGUAGE ASSISTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SR. DRUG SAFETY ASSOCIATE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> STATISTICIAN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> STAY-AT-HOME DAD </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> STEAMSHIP AGENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> STUDEY </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SUPERVISING PROGRAM DEVELOPMENT SPECIALI </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SURGEON </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> SURGICAL RESIDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TAX CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TAX EXAMINER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TELEVISION DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TELEVISION PRODUCER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TEMP </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TEMPORARY OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TEST ITEM WRITERER (SELF EMPLOYED </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> THEATER ARTIST/ TEACHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> THEATER GENERAL MANAGER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TOUR GUIDE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TOWN CLERK </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TOWN PLANNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TRADER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TRAFFIC REPORTER-RADIO </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TRAINEE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TREATMENT SUPPORT CO-ORDINATOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TV BROADCAST TECHNICAL </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> TV NEWS EXECUTIVE PRODUCER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> UNIVERSITY STAFF </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> URBAN PLANNER/ECONOMIC DEVELOPMENT PLANN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> VICE PRESIDENT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> VICE PRESIDENT / PROGRAM OFFICE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> VISUAL ARTIST </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> VMD </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> VOLUNTEER DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> VOLUNTEER MENTAL HEALTH WORKER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> VP SCIENTIFIC AFFAIRS / PHARMACEUTICAL C </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WAREHOUSE SUPERVISOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WEB COMMUNICATIONS </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WEBMASTER / PRINT DESIGNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WIG DESIGNER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERER / LECTURER / CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERER / WEB DESIGNER/ WEB-MASTER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERER &amp; DIRECTOR </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERER AND MANAGEMENT CONSULTANT </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERER/EDITOR/MUSICIAN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> WRITERER/MUSICIAN </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> YOGA TEACHER </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

```r
# 4d Count of respondents by country
Countrycnt <- count(procrastcomb, 'Countryres')
colnames(Countrycnt)[2] <- "Frequency"
Countrycnt <- Countrycnt[order(-Countrycnt$Frequency),]
kable(Countrycnt, "html", padding = 2, longtable = TRUE, row.names = FALSE) %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<thead><tr>
<th style="text-align:left;"> Countryres </th>
   <th style="text-align:right;"> Frequency </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;"> United States </td>
   <td style="text-align:right;"> 2598 </td>
  </tr>
<tr>
<td style="text-align:left;"> Canada </td>
   <td style="text-align:right;"> 237 </td>
  </tr>
<tr>
<td style="text-align:left;"> United Kingdom </td>
   <td style="text-align:right;"> 171 </td>
  </tr>
<tr>
<td style="text-align:left;"> Australia </td>
   <td style="text-align:right;"> 98 </td>
  </tr>
<tr>
<td style="text-align:left;"> India </td>
   <td style="text-align:right;"> 78 </td>
  </tr>
<tr>
<td style="text-align:left;"> Italy </td>
   <td style="text-align:right;"> 55 </td>
  </tr>
<tr>
<td style="text-align:left;"> Germany </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
<tr>
<td style="text-align:left;"> Brazil </td>
   <td style="text-align:right;"> 19 </td>
  </tr>
<tr>
<td style="text-align:left;"> Ireland </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
<tr>
<td style="text-align:left;"> Netherlands </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
<tr>
<td style="text-align:left;"> Sweden </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
<tr>
<td style="text-align:left;"> Norway </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
<tr>
<td style="text-align:left;"> France </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
<tr>
<td style="text-align:left;"> Japan </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
<tr>
<td style="text-align:left;"> Spain </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
<tr>
<td style="text-align:left;"> Finland </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
<tr>
<td style="text-align:left;"> New Zealand </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
<tr>
<td style="text-align:left;"> South Africa </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
<tr>
<td style="text-align:left;"> China </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
<tr>
<td style="text-align:left;"> Philippines </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
<tr>
<td style="text-align:left;"> Switzerland </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
<tr>
<td style="text-align:left;"> Belgium </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
<tr>
<td style="text-align:left;"> Denmark </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
<tr>
<td style="text-align:left;"> Greece </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
<tr>
<td style="text-align:left;"> Mexico </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
<tr>
<td style="text-align:left;"> Turkey </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
<tr>
<td style="text-align:left;"> Hong Kong </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
<tr>
<td style="text-align:left;"> Portugal </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
<tr>
<td style="text-align:left;"> Slovenia </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
<tr>
<td style="text-align:left;"> Poland </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> Romania </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
<tr>
<td style="text-align:left;"> Chile </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> Croatia </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> Malaysia </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> Singapore </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> Afghanistan </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> Algeria </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> Austria </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> Czech Republic </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> Ecuador </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> Uruguay </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
<tr>
<td style="text-align:left;"> Argentina </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Bulgaria </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Ghana </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Iran </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Malta </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Peru </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Saudi Arabia </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Ukraine </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Venezuela </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
<tr>
<td style="text-align:left;"> Albania </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Andorra </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Bahamas </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Barbados </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Bolivia </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Botswana </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Cyprus </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Dominican Republic </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Egypt </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Guyana </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Hungary </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Iceland </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Kenya </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Lithuania </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Luxembourg </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Macedonia </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Myanmar </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Nicaragua </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Pakistan </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Panama </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Qatar </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Russia </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> South Korea </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Sri Lanka </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Thailand </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> Vietnam </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

```r
#str(procrastcomb)
#4e matched perceptions
procrastcomb$UCONSPRO <- as.character(procrastcomb$UCONSPRO )
procrastcomb$OTHCONSPRO <- as.character(procrastcomb$OTHCONSPRO)
#where repsonses match
procrastmatch <- procrastcomb[which(procrastcomb$UCONSPRO == procrastcomb$OTHCONSPRO), ]
promatchsum <- count(procrastmatch, 'UCONSPRO')
colnames(promatchsum)[1] <- "Responses"
colnames(promatchsum)[2] <- "#Matches"
kable(promatchsum, "html", padding = 2, longtable = TRUE, row.names = FALSE, caption ="Personal perception procrastination responses that match others perceptions") %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<caption>Personal perception procrastination responses that match others perceptions</caption>
 <thead><tr>
<th style="text-align:left;"> Responses </th>
   <th style="text-align:right;"> #Matches </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;">  </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
<tr>
<td style="text-align:left;"> no </td>
   <td style="text-align:right;"> 405 </td>
  </tr>
<tr>
<td style="text-align:left;"> yes </td>
   <td style="text-align:right;"> 2151 </td>
  </tr>
</tbody>
</table>

```r
#where responses do not match
procrastmismatch <- procrastcomb[which(procrastcomb$UCONSPRO != procrastcomb$OTHCONSPRO), ]
promismatchsum <- count(procrastmismatch, 'UCONSPRO')
colnames(promismatchsum)[1] <- "Responses"
colnames(promismatchsum)[2] <- "#MisMatches"
kable(promismatchsum, "html", padding = 2, longtable = TRUE, row.names = FALSE, caption ="Personal perception procrastination responses that do not match others perceptions") %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
```

<table class="table table-striped" style="width: auto !important; ">
<caption>Personal perception procrastination responses that do not match others perceptions</caption>
 <thead><tr>
<th style="text-align:left;"> Responses </th>
   <th style="text-align:right;"> #MisMatches </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;">  </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
<tr>
<td style="text-align:left;"> no </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
<tr>
<td style="text-align:left;"> yes </td>
   <td style="text-align:right;"> 1005 </td>
  </tr>
</tbody>
</table>

To get to where the greatest need is for the productivity training analysis must be shown at the country level.  When top most procrastinating countries compared by two different procrastination survey indices there emerges that there are eight countries that are in both indices. These are the Dominican Republic, Qatar, Panama, Ecuador, Sri Lanka, Turkey, Uruguay, and Iran. Many of these countries are in the HDI Category high. This seems like they are countries that would benefit most from the centers.  If, the company wished to focus more on countries with higher disposable incomes it might be best to limit the data to HDI category Very High.  If this restriction is made the top countries would be Qatar, Austria, Poland, Germany, Hong Kong, and Iceland.


```r
#5b barchart of top 15 dp

top15dp <- aggregate(procrastcomb[, 62], by = list(procrastcomb$Countryres, procrastcomb$HDICAT), mean, na.rm=TRUE)
colnames(top15dp) <- c("Country", "HDICat", "DPmean")

#get the dp countries for very high hdi
top15dpvh <- subset(top15dp, HDICat=='Very High' )
top15dpvh <- top15dpvh[order(-top15dpvh$DPmean),]
#top15dpvh

top15dp <- top15dp[order(-top15dp$DPmean),]
#top15dp
top15dp <- top15dp[1:15,]
#top15dp

#write table to csv(6c)
#setwd("C:/Users/jjschued/Documents")
write.table(top15dp, file = "top15dp.csv", col.names = TRUE, row.names = FALSE, sep =",")

 ggplot(top15dp, aes(reorder(Country, DPmean),DPmean))+
 geom_bar(aes(fill=HDICat), stat= "identity", width = .4, position = position_dodge(width = .9))+
 ggtitle("Top 15 DP Mean Countries")+
 theme(plot.title = element_text(hjust = 0.5), legend.position="none",)+
 xlab("Country")+
 ylab("DP Means")+
 theme(axis.text.y=element_text(hjust = .5, vjust = .5))+
  theme_solarized()+
 coord_flip()
```

![](CaseStudy26306_files/figure-html/detaianalysis-1.png)<!-- -->

```r
#5c barchart of top 15 aip
top15aip <- aggregate(procrastcomb[, 63], by = list(procrastcomb$Countryres, procrastcomb$HDICAT), mean, na.rm=TRUE)
colnames(top15aip) <- c("Country", "HDICAT","AIPmean")
top15aip <- top15aip[order(-top15aip$AIPmean),]
# top15aip
#write table to csv(6c)
#setwd("C:/Users/jjschued/Documents")
write.table(top15aip, file = "top15aip.csv", col.names = TRUE, row.names = FALSE, sep =",")

#get the aip countries for very high hdi
top15aipvh <- subset(top15aip, HDICAT=='Very High' )
top15aipvh <-top15aipvh[order(-top15aipvh$AIPmean),]
#top15aipvh

top15aip <- top15aip[1:15,]
#top15aip

 ggplot(top15aip, aes(reorder(Country, AIPmean),AIPmean))+
 geom_bar(aes(fill=HDICAT), stat= "identity", width = .4, position = position_dodge(width = .9))+
 ggtitle("Top 15 AIP Mean Countries")+ 
 theme(plot.title = element_text(hjust = 0.5), legend.position="none",)+
 xlab("Country")+
 ylab("AIP Means")+ 
 theme(axis.text.y=element_text(hjust = .5, vjust = .5))+
 theme_solarized()+
 coord_flip()
```

![](CaseStudy26306_files/figure-html/detaianalysis-2.png)<!-- -->


WAM also requested supplemental analysis regarding how to target advertising for their series of trainings.  Their interest was for ages, income, life satisfaction, and HDI categories of the targeted countries. The best relationship seems to be income to age.  There is more income at older ages. Other relationships were not consequential. Therefore, it is suggested to examine targeting older individuals. 



```r
 #5d relationship between age and income
 
 ggplot(procrastcomb, aes(x= procrastcomb$Age, y=procrastcomb$AnnualIncome))+
 geom_point(aes(col = procrastcomb$Gender))+
   guides(col=guide_legend(title="Gender"))+
geom_smooth(method=lm)+
 ggtitle("Age and Income Relationship")+ 
 theme(plot.title = element_text(hjust = 0.5), legend.position="none",)+
 xlab("Age")+
 ylab("Income")+ 
 theme(axis.text.y=element_text(hjust = .5, vjust = .5))+
 theme_solarized()
```

```
## Warning: Removed 324 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 324 rows containing missing values (geom_point).
```

![](CaseStudy26306_files/figure-html/relationships-1.png)<!-- -->

```r
 #5e relationship between Life satisfaction and HDI
 
 ggplot(procrastcomb, aes(x= procrastcomb$HDI, y=procrastcomb$swlsmean))+
 geom_point()+
geom_smooth(method=lm)+
 ggtitle("HDI and Life Satisfaction relationship")+ 
 theme(plot.title = element_text(hjust = 0.5), legend.position="none",)+
 xlab("HDI")+
 ylab("Life Satisfaction Mean")+ 
 theme(axis.text.y=element_text(hjust = .5, vjust = .5))+
 theme_solarized() 
```

```
## Warning: Removed 6 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 6 rows containing missing values (geom_point).
```

![](CaseStudy26306_files/figure-html/relationships-2.png)<!-- -->

```r
 #HDI cat and life satisfaction

 ggplot(procrastcomb, aes(x= procrastcomb$swlsmean, y=procrastcomb$HDICAT))+
 geom_bar(aes(fill=HDICAT), stat= "identity", width = .4, position = position_dodge(width = .9))+
 ggtitle("HDI and Life Satisfaction relationship")+
 theme(plot.title = element_text(hjust = 0.5), legend.position="none",)+
 xlab("Life Satisfaction Mean")+
 ylab("HDICat")+
 theme(axis.text.y=element_text(hjust = .5, vjust = .5))
```

```
## Warning: position_dodge requires non-overlapping x intervals
```

```
## Warning: Removed 6 rows containing missing values (geom_bar).
```

![](CaseStudy26306_files/figure-html/relationships-3.png)<!-- -->

```r
 theme_solarized()
```

```
## List of 59
##  $ line                 :List of 6
##   ..$ colour       : Named chr "#93a1a1"
##   .. ..- attr(*, "names")= chr "rebase01"
##   ..$ size         : num 0.5
##   ..$ linetype     : num 1
##   ..$ lineend      : chr "butt"
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  $ rect                 :List of 5
##   ..$ fill         : Named chr "#fdf6e3"
##   .. ..- attr(*, "names")= chr "rebase03"
##   ..$ colour       : Named chr "#93a1a1"
##   .. ..- attr(*, "names")= chr "rebase01"
##   ..$ size         : num 0.5
##   ..$ linetype     : num 1
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
##  $ text                 :List of 11
##   ..$ family       : chr ""
##   ..$ face         : chr "plain"
##   ..$ colour       : Named chr "#93a1a1"
##   .. ..- attr(*, "names")= chr "rebase01"
##   ..$ size         : num 12
##   ..$ hjust        : num 0.5
##   ..$ vjust        : num 0.5
##   ..$ angle        : num 0
##   ..$ lineheight   : num 0.9
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : logi FALSE
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.title.x         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 6 0 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.title.x.top     :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 0
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 6 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.title.y         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 1
##   ..$ angle        : num 90
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 6 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.title.y.right   :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 0
##   ..$ angle        : num -90
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 0 6
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text            :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : chr "grey30"
##   ..$ size         :Class 'rel'  num 0.8
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.x          :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 2.4 0 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.x.top      :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : num 0
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 2.4 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.y          :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : num 1
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 2.4 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.y.right    :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : num 0
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 0 2.4
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.ticks           :List of 6
##   ..$ colour       : Named chr "#93a1a1"
##   .. ..- attr(*, "names")= chr "rebase01"
##   ..$ size         : NULL
##   ..$ linetype     : NULL
##   ..$ lineend      : NULL
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  $ axis.ticks.length    :Class 'unit'  atomic [1:1] 3
##   .. ..- attr(*, "valid.unit")= int 8
##   .. ..- attr(*, "unit")= chr "pt"
##  $ axis.line            :List of 6
##   ..$ colour       : Named chr "#93a1a1"
##   .. ..- attr(*, "names")= chr "rebase01"
##   ..$ size         : NULL
##   ..$ linetype     : num 1
##   ..$ lineend      : NULL
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  $ axis.line.x          : NULL
##  $ axis.line.y          : NULL
##  $ legend.background    :List of 5
##   ..$ fill         : NULL
##   ..$ colour       : logi NA
##   ..$ size         : NULL
##   ..$ linetype     : NULL
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
##  $ legend.margin        :Classes 'margin', 'unit'  atomic [1:4] 0.2 0.2 0.2 0.2
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ legend.spacing       :Class 'unit'  atomic [1:1] 0.4
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ legend.spacing.x     : NULL
##  $ legend.spacing.y     : NULL
##  $ legend.key           :List of 5
##   ..$ fill         : chr "white"
##   ..$ colour       : logi NA
##   ..$ size         : NULL
##   ..$ linetype     : num 0
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
##  $ legend.key.size      :Class 'unit'  atomic [1:1] 1.2
##   .. ..- attr(*, "valid.unit")= int 3
##   .. ..- attr(*, "unit")= chr "lines"
##  $ legend.key.height    : NULL
##  $ legend.key.width     : NULL
##  $ legend.text          :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         :Class 'rel'  num 0.8
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ legend.text.align    : NULL
##  $ legend.title         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : num 0
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ legend.title.align   : NULL
##  $ legend.position      : chr "right"
##  $ legend.direction     : NULL
##  $ legend.justification : chr "center"
##  $ legend.box           : NULL
##  $ legend.box.margin    :Classes 'margin', 'unit'  atomic [1:4] 0 0 0 0
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ legend.box.background: list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ legend.box.spacing   :Class 'unit'  atomic [1:1] 0.4
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ panel.background     :List of 5
##   ..$ fill         : Named chr "#fdf6e3"
##   .. ..- attr(*, "names")= chr "rebase03"
##   ..$ colour       : Named chr "#93a1a1"
##   .. ..- attr(*, "names")= chr "rebase01"
##   ..$ size         : NULL
##   ..$ linetype     : NULL
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
##  $ panel.border         : list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  $ panel.spacing        :Class 'unit'  atomic [1:1] 6
##   .. ..- attr(*, "valid.unit")= int 8
##   .. ..- attr(*, "unit")= chr "pt"
##  $ panel.spacing.x      : NULL
##  $ panel.spacing.y      : NULL
##  $ panel.grid.major     :List of 6
##   ..$ colour       : Named chr "#eee8d5"
##   .. ..- attr(*, "names")= chr "rebase02"
##   ..$ size         : NULL
##   ..$ linetype     : NULL
##   ..$ lineend      : NULL
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  $ panel.grid.minor     :List of 6
##   ..$ colour       : Named chr "#eee8d5"
##   .. ..- attr(*, "names")= chr "rebase02"
##   ..$ size         : num 0.25
##   ..$ linetype     : NULL
##   ..$ lineend      : NULL
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  $ panel.ontop          : logi FALSE
##  $ plot.background      :List of 5
##   ..$ fill         : NULL
##   ..$ colour       : logi NA
##   ..$ size         : NULL
##   ..$ linetype     : num 0
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
##  $ plot.title           :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         :Class 'rel'  num 1.2
##   ..$ hjust        : num 0
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 7.2 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ plot.subtitle        :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         :Class 'rel'  num 0.9
##   ..$ hjust        : num 0
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 0 5.4 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ plot.caption         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         :Class 'rel'  num 0.9
##   ..$ hjust        : num 1
##   ..$ vjust        : num 1
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 5.4 0 0 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ plot.margin          :Classes 'margin', 'unit'  atomic [1:4] 6 6 6 6
##   .. ..- attr(*, "valid.unit")= int 8
##   .. ..- attr(*, "unit")= chr "pt"
##  $ strip.background     :List of 5
##   ..$ fill         : chr "grey85"
##   ..$ colour       : chr "grey20"
##   ..$ size         : NULL
##   ..$ linetype     : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_rect" "element"
##  $ strip.placement      : chr "inside"
##  $ strip.text           :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : chr "grey10"
##   ..$ size         :Class 'rel'  num 0.8
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ strip.text.x         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 6 0 6 0
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ strip.text.y         :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : num -90
##   ..$ lineheight   : NULL
##   ..$ margin       :Classes 'margin', 'unit'  atomic [1:4] 0 6 0 6
##   .. .. ..- attr(*, "valid.unit")= int 8
##   .. .. ..- attr(*, "unit")= chr "pt"
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi TRUE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ strip.switch.pad.grid:Class 'unit'  atomic [1:1] 0.1
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ strip.switch.pad.wrap:Class 'unit'  atomic [1:1] 0.1
##   .. ..- attr(*, "valid.unit")= int 1
##   .. ..- attr(*, "unit")= chr "cm"
##  $ title                :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : Named chr "#657b83"
##   .. ..- attr(*, "names")= chr "rebase0"
##   ..$ size         : NULL
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ panel.grid           :List of 6
##   ..$ colour       : Named chr "#eee8d5"
##   .. ..- attr(*, "names")= chr "rebase02"
##   ..$ size         : NULL
##   ..$ linetype     : NULL
##   ..$ lineend      : NULL
##   ..$ arrow        : logi FALSE
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_line" "element"
##  - attr(*, "class")= chr [1:2] "theme" "gg"
##  - attr(*, "complete")= logi TRUE
##  - attr(*, "validate")= logi TRUE
```
In conclusion, though beyond the United States and a few other countries sample sizes are small the results suggest that there are markets that warrant additional analysis outside the scope of this project.  Those include: central Europe, Hong Kong, and Qatar.  Further surveys of employment age individuals should be gathered for all HDI Category-Very High to make better decisions about placement of their centers. It is important to place the centers in high-income areas in order to maximize exposure to customers with disposable income.   

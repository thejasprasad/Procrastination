# Initialize Libraries
library(data.table)
library("rvest") # Great for grabbing and parsing HTML
library("dplyr") # Easy way for transformations of data.frames for summarization
library("tidyr") # Nice way to arrange data
library("ggplot2") # Excellent for visuals

# Importing the dataset

#dt -> Data Table
procrasti_dt = data.table(read.table('Procrastination.csv',sep = ",",header = TRUE, as.is = TRUE, check.names=FALSE, quote = "\""))
str(procrasti_dt)

#?read.csv
# 2.a No Of Rows & No Of Columns - reduce to 12 characters

nrow(procrasti_dt)
ncol(procrasti_dt)

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
head(procrasti_dt$CurrPosYrs)
head(max(procrasti_dt$CurrPosYrs) > 70, na.rm = TRUE)
max(procrasti_dt[, CurrPosYrs],na.rm = TRUE)

# 2.c 2 No Of Sons fix the values and chnge the data type to integers

procrasti_dt[ NoOfSons == "Male", NoOfSons := "1" ,  ]
procrasti_dt[ NoOfSons == "Female", NoOfSons := "2" ,  ]

procrasti_dt[ , NoOfSons := .(as.integer(NoOfSons)),  ]

#Test the results below
head(procrasti_dt$NoOfSons)
str(procrasti_dt)

# 2.c 3 Treat 0 as NA in the CountryOfRes

# Get count 1st
procrasti_dt[ CountryOfRes == "0" , .N, ]
#procrasti_dt[ CountryOfRes == "NA", .N, ]
procrasti_dt[ CountryOfRes == "", .N, ]

# Change to NA
procrasti_dt[ CountryOfRes == "0", CountryOfRes := NA, ]
procrasti_dt[ CountryOfRes == "", CountryOfRes := NA, ]

# Test Results Get count --should be 0
procrasti_dt[ CountryOfRes == "0", .N, ]


# 2.c 4 Treat 0 and 'please specify'as NA in the Current Occupation column, then categorize the jobs

# Get count 1st
procrasti_dt[ CurrentOccup == "0", .N, ]
procrasti_dt[ CurrentOccup == "please specify", .N, ]
procrasti_dt[ CurrentOccup == "", .N, ]

# Change to NA
procrasti_dt[ CurrentOccup == "0", CurrentOccup := NA, ]
procrasti_dt[ CurrentOccup == "please specify", CurrentOccup := NA, ]
procrasti_dt[ CurrentOccup == "", CurrentOccup := NA, ]

# Test Results Get count --should be 0
procrasti_dt[ CurrentOccup == "0", .N, ]
procrasti_dt[ CurrentOccup == "please specify", .N, ]
procrasti_dt[ CurrentOccup == "", .N, ]

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

procrasti_dt[ CurrentOccup == "Teacher", .N,]

# 2.d Fix the data types

str(procrasti_dt)

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

nrow(hdit_cntry_table)
# remove World from above table

# Write to a csv file.
write.csv(hdit_cntry_table , file = "HDI_Country.csv", row.names = FALSE)

#str(hdit_cntry_table8)

# 3.c Merge 2 DT

merge_dt <- merge( procrasti_dt,hdit_cntry_table, by.x="CountryOfRes", by.y="Country", all=TRUE)

# 4.a Remove all under age 18

merge_dt <- merge_dt[ Age >18, , ]
# Remove 65
merge_dt <- merge_dt[ Age <65, , ]


#4.b
des_stats <- merge_dt[ , .(Age, AnnualIncome, as.numeric(HDI),DPMean,AIPMean,GPMean,SWLSMean), ]

summary(des_stats)
hist(des_stats$Age, main = "AGE HIST", xlab = "AGE" )
hist(des_stats$DPMean , main = "DPMean HIST", xlab = "DPMean" )
?hist

nrow(des_stats$Age)

nrow(des_stats$AnnualIncome)

str(des_stats)
#TO CHECK NA COUNT
des_stats[, sum(is.na(Age)), ]
des_stats[, sum(is.na(AnnualIncome)), ]
des_stats[, sum(is.na(HDI)), ]
des_stats[, sum(is.na(DPMean)), ]


#4.c frequencies of variables - dplyr function --- not matching with Joe

freq_gen <- summarise(group_by(merge_dt,Gender) , GenderFreq =n())
freq_gen <- freq_gen[2:3, ]

freq_WorkStatus <- summarise(group_by(merge_dt,WorkStatus) , WorkStatusFreq =n())
freq_WorkStatus <- freq_WorkStatus[2:6, ]

freq_CurrentOccup <- summarise(group_by(merge_dt,CurrentOccup) , CurrentOccupFreq =n())

str(merge_dt)
#?summarise

#4.d Count Participiants per country desc

freq_parti <- summarise(group_by(merge_dt,CountryOfRes) , NoOfParticipants =n())
freq_parti <- data.table(freq_parti)
freq_parti <- freq_parti [ order(-NoOfParticipants)]

?order

#4.e Count of procra_yes_yes and procra_no_no

nrow(merge_dt[ SlfConProCra == "yes" & OtrConProCra == "yes" , ,] )

nrow(merge_dt[ SlfConProCra == "no" & OtrConProCra == "no" , ,] )

#nrow(merge_dt[ SlfConProCra == "yes" & OtrConProCra == "no" , ,] )
#nrow(merge_dt[ SlfConProCra == "no" & OtrConProCra == "yes" , ,] )
#nrow(merge_dt[ SlfConProCra == "" & OtrConProCra == "" , ,] )
#nrow(merge_dt[ SlfConProCra == "" & OtrConProCra == "0" , ,] )

# 5.a 
# 5.b

#Create a colorful bar chart that shows the top 15 nations avg HDI score

head(merge_dt)

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


# Below countries appear in AIP and DP score top 15 lists
# India, Australia, Canada, Belgium, Brazil, Finland, France, Ireland

# 5.d Relationship Age and Income

#Linear reg

regressor = lm(formula = AnnualIncome ~ Age, data=merge_dt )
regressor

summary(regressor)

#predict
y_pred = predict(regressor, newdata = merge_dt )
y_pred


ggplot() +
  geom_point(data = merge_dt, aes(x= merge_dt$Age, y= merge_dt$AnnualIncome),
             colour='red', na.rm = TRUE,size = .75)+
  geom_line(aes(x= merge_dt$Age,y= predict(regressor, newdata = merge_dt )),
            colour='blue',na.rm = TRUE)+
  ggtitle('Age vs Anual Income')+
  xlab('Age')+
  ylab('Income')


# 5.e Relationship Life Satisfaction and HDI
#Linear reg

regressor = lm(formula =  SWLS3SatisLf ~ HDI , data=merge_dt )
regressor

summary(regressor)

#predict
y_pred = predict(regressor, newdata = merge_dt )
y_pred


ggplot() +
  geom_point(data = merge_dt, aes(x= merge_dt$HDI, y= merge_dt$SWLS3SatisLf),
             colour='red', na.rm = TRUE,size = .75)+
  geom_line(aes(x= merge_dt$HDI,y= predict(regressor, newdata = merge_dt )),
            colour='blue',na.rm = TRUE)+
  ggtitle('Life Satisfaction vs HDI')+
  xlab('HDI')+
  ylab('Life Satisfaction')


# Bar plot HDI Category

merge_dt_hdi_cat <- data.table(na.omit(merge_dt[ , .(SWLS3SatisLf,HDICategory ), ]))

ggplot(data = merge_dt_hdi_cat, aes(SWLS3SatisLf , HDICategory))+
  geom_bar(aes(fill = HDICategory), stat = "identity")+
  #coord_flip() +
  ggtitle(" HDI Category")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Life Satisfaction")+
  ylab ("HDI Category")


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
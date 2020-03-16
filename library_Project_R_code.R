dev.off()     # Clear the graph window
cat('\014')   # Clear the console  
rm(list=ls()) # Clear all user objects from the environment
setwd('C:/Users/avind/Desktop/iconsylt/Project 1')

install.packages("readxl")
install.packages("xlsx")
install.packages("bizdays")
library(xlsx)
library(readxl)
library(tidyverse)
library(stringr)
library(reshape2)
library(lubridate)
library(bizdays)

masterdata<-read.csv("masterdata.csv")
View(masterdata)
any(is.na(masterdata$ItemBarcode))
any(is.na(masterdata$TransitSentBranchName))
any(is.na(masterdata$TransitRcvBranchName))
any(is.na(masterdata$TransitSentBranchAbbr))
any(is.na(masterdata$TransitRcvBranchAbbr))#has NA
any(is.na(masterdata$InTransitSentDate))
any(is.na(masterdata$InTransitRecvdDate))
any(is.na(masterdata$ItemStatusDescr))

testingdata<-masterdata
View(testingdata)

#For sent dates
y <- as.data.frame(str_split_fixed(testingdata$InTransitSentDate, " ", 2))
colnames(y)[1]<-"SentDate"
colnames(y)[2]<-"SentTime"
View(y)
testingdata$SentDate<-y$SentDate
testingdata$SentTime<-y$SentTime  

testingdata$SentDate<-parse_date_time(testingdata$SentDate,orders = "mdy")
typeof(testingdata$SentDate)
typeof(testingdata$SentTime)

#for received dates
x <- as.data.frame(str_split_fixed(testingdata$InTransitRecvdDate, " ", 2))
colnames(x)[1]<-"RecievedDate"
colnames(x)[2]<-"RecievedTime"
View(x)
testingdata$RecievedDate<-x$RecievedDate
testingdata$RecievedTime<-x$RecievedTime 

testingdata$RecievedDate<-parse_date_time(testingdata$RecievedDate,orders = "mdy")

cal<-create.calendar(name="mycal", weekdays = c("saturday","sunday"))
testingdata$diff<-0
index<-seq(1,nrow(testingdata),2)
testingdata$diff[index]<-bizdays(testingdata$SentDate[index], testingdata$RecievedDate[index], cal)

testingdata$Sentday <- weekdays(as.Date(testingdata$SentDate))

View(testingdata)
sum(is.na(testingdata$TransitSentBranchName))
sum(is.na(testingdata$TransitSentBranchAbbr))
sum(is.na(testingdata$TransitRcvBranchAbbr))#4 NA values
sum(is.na(testingdata$InTransitSentDate))
sum(is.na(testingdata$InTransitRecvdDate))
sum(is.na(testingdata$ItemBarcode))
sum(is.na(testingdata$ItemStatusDescr))
sum(is.na(testingdata$SentDate))#1 NA value
sum(is.na(testingdata$SentTime))
sum(is.na(testingdata$RecievedDate))#2151 NA values
sum(is.na(testingdata$RecievedTime))

testingdata <- testingdata [(!(testingdata$TransitSentBranchAbbr == "BR")),]
testingdata <- testingdata [(!(testingdata$TransitSentBranchAbbr == "NS")),]
testingdata <- testingdata [(!(testingdata$TransitSentBranchAbbr == "CC")),]
na.omit(testingdata$RecievedDate)

testingdata$colorcode <- 0
testingdata$colorcode<-ifelse(testingdata$TransitSentBranchAbbr== "BV"|testingdata$TransitSentBranchAbbr== "BR"|
                                testingdata$TransitSentBranchAbbr== "CC"|testingdata$TransitSentBranchAbbr== "LV"|
                                testingdata$TransitSentBranchAbbr== "NS"|testingdata$TransitSentBranchAbbr== "NE"|
                                testingdata$TransitSentBranchAbbr== "SA"|
                                testingdata$TransitSentBranchAbbr== "WH","Yellow", 
                              ifelse(testingdata$TransitSentBranchAbbr== "DE"| testingdata$TransitSentBranchAbbr== "ES"|
                                       testingdata$TransitSentBranchAbbr== "FY"|testingdata$TransitSentBranchAbbr== "LF"|
                                       testingdata$TransitSentBranchAbbr== "MN"| testingdata$TransitSentBranchAbbr== "MI"|
                                       testingdata$TransitSentBranchAbbr== "PA"|testingdata$TransitSentBranchAbbr== "PT"| 
                                       testingdata$TransitSentBranchAbbr== "SL"|testingdata$TransitSentBranchAbbr== "TU", "Blue","Red"))

##write.csv(testingdata,"testingdata.csv")

###--------CODE TRIALS-----------------#####-------Please IGNORE-----###########
#data<-read.csv2("testdata.xls")
#View(data)
#names(data)[names(data) == "InTransitSentDate.TransitSentBranchAbbr.TransitSentBranchName.TransitRcvBranchAbbr.TransitRcvBranchName.InTransitRecvdDate.ItemBarcode"] <- "sepal_length"
#write.csv(data, file="new1.csv")

#data<-str_split_fixed(data$sepal_length,"\\s",20)

#data<-read.csv2(file="try1.csv", sep="\t")
#view(data)

#final<-read.csv("ItemLists_76449csv.csv",header = TRUE, sep=",")
#View(final)
#write.xlsx(final,"C:/Users/Shripad/Desktop/MS/iConsult/fullfinal.xlsx")

#testingdata$SentDate<-colsplit(testingdata$InTransitSentDate," ",c("TransitSentDate","TransitSentTime"))
#View(testingdata)

#ptime<-as.POSIXlt(testingdata$InTransitSentDate)

#testingdata$SentTime<-format(as.POSIXct(testingdata$SentTime), format ="%H:%M")
#testingdata$SentTime<-format(testingdata$SentTime,"%H:%M:%S")
#testingdata$SentTime<-strptime(testingdata$SentTime,"%H:%M")
#install.packages("anytime")
#library(anytime)
#testingdata$SentTime<-anytime(testingdata$InTransitSentDate)

#testingdata$SentDate<-as.Date(testingdata$SentDate, format="%m/%d/%Y")

#testingdata$SentDate<-as.Date(testingdata $SentDate,format="%m/%d/%Y")#causing issue
#View(testingdata$InTransitSentDate)


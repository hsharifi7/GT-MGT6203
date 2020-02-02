#R code used in Module: Introduction and Measuring Risk and Return

#Installing packages as needed

#NOTE: This statement essentially checks if PerformanceAnalytics package is available
#locally in your R library distribution. If not, it will install it and then include it
#as a part of this code, so that we can use its functions and features
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
if (!require(xts)) install.packages("xts")
if (!require(lubridate)) install.packages("lubridate")


#Package Details
#1)Performanceanalytics: A very useful package for investment and financial performance and risk 
#analytics. Official Documentation: https://www.rdocumentation.org/packages/PerformanceAnalytics/versions/1.5.3
#Presentation Deck by Package Founders: http://past.rinfinance.com/RinFinance2009/presentations/PA%20Workshop%20Chi%20RFinance%202009-04.pdf
#Quick Video on calculating returns: https://www.youtube.com/watch?v=0rAVPUNf9yI

#2) xts: xts is a useful packge useful in time-series analysis. We use xts package here since
#PerformanceAnalytics functions usually require xts objects (time-series of prices etc.) rather than simple
#lists of prices for more accurate performance evaluation

#3) lubridate: lubridate is a date mainpulation package. We use mdy() function of lubridate to standardize dates of our data 
#Useful Resource: https://raw.githubusercontent.com/rstudio/cheatsheets/master/lubridate.pdf

# calling libraries
library(PerformanceAnalytics)
library(xts)
library(lubridate)


############### Video 1: Simple and Compound Interest

#Before running this code - please make sure that contrafund.csv is in your working directory
#use getwd() to find out your working directory and setwd() to set it to any directory you want

#load data and create xts dataset
fund <- read.csv("contrafund.csv")
#converting dates to standard YYYY-MM-DD format
fund$Date <- mdy(fund$Date)
#Sorting data by dates
fund2<- fund[order(fund$Date),]

#create an xts dataset
All.dat <- xts(fund2[,-1],order.by = fund2[,1],)


#Calculate Compound Return for the fund across all the data
Return.cumulative(All.dat$ContraRet,geometric = TRUE)


#Cumulative Returns chart over time
#Check chart in Plots Tab on bottom right in R Studio
chart.CumReturns(All.dat$ContraRet,wealth.index = FALSE, geometric = TRUE)



############### Video 2: Measuring Risk

#Descriptive Statistics of the fund returns
#IMPORTANT NOTE - Arithmetic mean and standard deviation of returns are reported incorrectly
#The correct values as noticed when this code is run are:
#Arithmetic Mean Return = 1.17% and Standard Deviation = 4.32% per month
table.Stats(All.dat$ContraRet)


#Beta and R-squared estimation: 
Mod1 = lm(ContraRet ~ Market.Return,data= All.dat)
summary(Mod1)

#As seen in model, beta = 0.9005 , Adj. R-sq = 0.8313


#Drawdowns
chart.Drawdown(All.dat$ContraRet)
table.Drawdowns(All.dat$ContraRet,top = 5,digits = 4)

##End of Code

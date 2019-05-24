#install.packages(
 # c("ggfortify", "changepoint",
 #   "strucchange", "ggpmisc", "dplyr")
#)
#install.packages("devtools")
#devtools::install_github("robjhyndman/forecast")
#install.packages("knitr")
#For some reason, it did not work when I tried to download it, so I had to manually download it from Tools -> Install packages. I'm sorry!
#install.packages("Metrics")
#install.packages("dplyr")
#install.packages("zoo")
#install.packages("BBmisc")

library(BBmisc)
library(Metrics)
library(forecast)
library(knitr)
library(magrittr)
library(stats)
library(readxl)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(zoo)
#Read in the data from the excel sheets
HawaiiUnemploymentData <- read_excel("HIHAWA0URN.xls")
HonoluluUnemploymentData <-read_excel("HIHONO7URN.xls")
LasVegasUnemploymentData<-read_excel("LASV832URN.xls")

#Plot these bad boys
HawaiiPlot<- ggplot(data = HawaiiUnemploymentData, aes(x=observation_date, y = HIHAWA0URN)) +
  geom_line(color = "#FF0027", size = 1.5) + stat_smooth(
    color = "#000000", fill = "#000000",
    method = "loess"
  ) + ggtitle("Unemployment rate for Hawaii Island") +
  xlab("Month and Year") + ylab("Unemployment Percent")
#Display the plots
HawaiiPlot 

#Plot Honolulu data
HonoluluPlot<- ggplot(data = HonoluluUnemploymentData, aes(x=observation_date, y = HIHONO7URN)) +
  geom_line(color = "#207D5E", size = 1.5) + stat_smooth(
    color = "#000000", fill = "#000000",
    method = "loess") + ggtitle("Unemployment rate for Honolulu Island") +
  xlab("Month and Year") + ylab("Unemployment Percent")
HonoluluPlot 

#Plot Las Vegas data 
LasVegasPlot<- ggplot(data = LasVegasUnemploymentData, aes(x=observation_date, y = LASV832URN)) +
  geom_line(color = "#0000FF", size = 1.5) + stat_smooth(
    color = "#000000", fill = "#000000",
    method = "loess") + ggtitle("Unemployment rate for Las Vegas") +
  xlab("Month and Year") + ylab("Unemployment Percent")
LasVegasPlot 

#Plot them all together
HIHLVPlot <- ggplot() + 
  geom_line(data = HonoluluUnemploymentData, aes(x=observation_date, y = HIHONO7URN), color = "#207D5E", size = 1.0) +
  geom_line(data = HawaiiUnemploymentData, aes(x=observation_date, y = HIHAWA0URN), color = "#FF0027", size = 1.0) +  
  geom_line(data = LasVegasUnemploymentData, aes(x=observation_date, y = LASV832URN), color = "#0000FF", size = 1.0) + ggtitle("Unemployment Rate Comparison") +
  xlab("Month and Year") + ylab("Unemployment Percent")
HIHLVPlot

#Split into training and test data (5 years and 10 years)
#I will be using the 5 year data to train and forecast 1 year into the future.
#I will be using the 10 year data to forecast 2 years into the future
Hawaii.Train5 <- HawaiiUnemploymentData[1:61,]
Hawaii.Train10 <- HawaiiUnemploymentData[1:121,]
Hawaii.Test5 <- HawaiiUnemploymentData[62:73,]
Hawaii.Test10 <- HawaiiUnemploymentData[122:145,]

Honolulu.Train5 <- HonoluluUnemploymentData[1:61,]
Honolulu.Train10 <- HonoluluUnemploymentData[1:121,]
Honolulu.Test5 <- HonoluluUnemploymentData[62:73,]
Honolulu.Test10 <- HonoluluUnemploymentData[122:145,]

LasVegas.Train5 <- LasVegasUnemploymentData[1:61,]
LasVegas.Train10 <- LasVegasUnemploymentData[1:121,]
LasVegas.Test5 <- LasVegasUnemploymentData[62:73,]
LasVegas.Test10 <- LasVegasUnemploymentData[122:145,]


#Create Time Series Objects (Training set for 5 years)
Hawaii.Train5.ts <- ts(Hawaii.Train5$HIHAWA0URN, start = c(1990, 1), end = c(1994, 12), frequency =  12)
Honolulu.Train5.ts <- ts(Honolulu.Train5$HIHONO7URN, start = c(1990, 1), end = c(1994, 12), frequency =  12)
LasVegas.Train5.ts <- ts(LasVegas.Train5$LASV832URN, start = c(1990, 1), end = c(1994, 12), frequency =  12)

#Create Time Series Objects (Training set for 10 years)
Hawaii.Train10.ts <- ts(Hawaii.Train10$HIHAWA0URN, start = c(1990, 1), end = c(2000, 1), frequency =  12)
Honolulu.Train10.ts <- ts(Honolulu.Train10$HIHONO7URN, start = c(1990, 1), end = c(2000, 1), frequency =  12)
LasVegas.Train10.ts <- ts(LasVegas.Train10$LASV832URN, start = c(1990, 1), end = c(2000, 1), frequency =  12)

#Using 5 years to train for 1 year
naive.Hawaii.Train5 <- naive(Hawaii.Train5.ts, h=12)
naive.Honolulu.train5 <-naive(Honolulu.Train5.ts, h=12)
naive.LasVegas.Train5 <-naive(LasVegas.Train5.ts, h=12)

#Naive training forecast for 2 years
naive.Hawaii.Train10 <- naive(Hawaii.Train10.ts, h=24)
naive.Honolulu.train10 <-naive(Honolulu.Train10.ts, h=24)
naive.LasVegas.Train10 <-naive(LasVegas.Train10.ts, h=24)

#Testing the data using the RMSE method for 5 years
rmsetestNaiveHawaii5 <- rmse(Hawaii.Test5$HIHAWA0URN, naive.Hawaii.Train5$mean)
rmsetestNaiveHawaii5 #Result is 1.584035

rmsetestNaiveHonolulu5 <- rmse(Honolulu.Test5$HIHONO7URN, naive.Honolulu.train5$mean)
rmsetestNaiveHonolulu5 # Result is 1.261613

rmsetestNaiveLasVegas5 <- rmse(LasVegas.Test5$LASV832URN, naive.LasVegas.Train5$mean)
rmsetestNaiveLasVegas5 #Value is 0.2901149

#Testing data for 10 years
rmsetestNaiveHawaii10 <- rmse(Hawaii.Test10$HIHAWA0URN, naive.Hawaii.Train10$mean)
rmsetestNaiveHawaii10 #Result is 0.458712

rmsetestNaiveHonolulu10 <- rmse(Honolulu.Test10$HIHONO7URN, naive.Honolulu.train10$mean)
rmsetestNaiveHonolulu10 # Result is 0.59196

rmsetestNaiveLasVegas10 <- rmse(LasVegas.Test10$LASV832URN, naive.LasVegas.Train10$mean)
rmsetestNaiveLasVegas10 #Value is 0.989739

#Now we will try the SES Method for 5 years and forecast 1 year
SES.Hawaii.Train5 <- ses(Hawaii.Train5.ts, h = 12, alpha = 0.9, initial = "simple")
rmsetestSESHawaii5 <- rmse(Hawaii.Test5$HIHAWA0URN, SES.Hawaii.Train5$mean)
rmsetestSESHawaii5 # Result is 1.495585

SES.Honolulu.Train5 <- ses(Honolulu.Train5.ts, h = 12, alpha = 0.9, initial = "simple")
rmsetestSESHonolulu5 <- rmse(Honolulu.Test5$HIHONO7URN, SES.Honolulu.Train5$mean)
rmsetestSESHonolulu5 # Result is 1.211121

SES.LasVegas.Train5 <- ses(LasVegas.Train5.ts, h = 12, alpha = 0.9, initial = "simple")
rmsetestSESLasVegas5 <- rmse(LasVegas.Test5$LASV832URN, SES.LasVegas.Train5$mean)
rmsetestSESLasVegas5 # Result is 0.2777311

#Now we will use SES Method for 10 years to forecast 2 years
SES.Hawaii.Train10 <- ses(Hawaii.Train10.ts, h = 24, alpha = 0.9, initial = "simple")
rmsetestSESHawaii10 <- rmse(Hawaii.Test10$HIHAWA0URN, SES.Hawaii.Train10$mean)
rmsetestSESHawaii10 # Result is 0.4781746

SES.Honolulu.Train10 <- ses(Honolulu.Train10.ts, h = 24, alpha = 0.9, initial = "simple")
rmsetestSESHonolulu10 <- rmse(Honolulu.Test10$HIHONO7URN, SES.Honolulu.Train10$mean)
rmsetestSESHonolulu10 # Result is 0.5551378

SES.LasVegas.Train10 <- ses(LasVegas.Train10.ts, h = 24, alpha = 0.9, initial = "simple")
rmsetestSESLasVegas10 <- rmse(LasVegas.Test10$LASV832URN, SES.LasVegas.Train10$mean)
rmsetestSESLasVegas10 # Result is 1.02343

#Holt-Winters for 5 years and predicting 1 year
Holt.Winters.Hawaii.Train5 <- HoltWinters(Hawaii.Train5.ts, beta=TRUE, gamma=TRUE)
rmsetestHoltHawaii5 <- rmse(Hawaii.Test5$HIHAWA0URN, predict(Holt.Winters.Hawaii.Train5, n.ahead = 12))
rmsetestHoltHawaii5 # Result is 3.65132

Holt.Winters.Honolulu.Train5 <- HoltWinters(Honolulu.Train5.ts, beta=TRUE, gamma=TRUE)
rmsetestHoltHonolulu5 <- rmse(Honolulu.Test5$HIHONO7URN, predict(Holt.Winters.Honolulu.Train5, n.ahead = 12))
rmsetestHoltHonolulu5 # Result is 2.613715

Holt.Winters.LasVegas.Train5 <- HoltWinters(LasVegas.Train5.ts, beta=TRUE, gamma=TRUE)
rmsetestHoltLasVegas5 <- rmse(LasVegas.Test5$LASV832URN, predict(Holt.Winters.LasVegas.Train5, n.ahead = 12))
rmsetestHoltLasVegas5 # Result is 2.440842

#Holt-Winters for 10 years and predicting 2 year
Holt.Winters.Hawaii.Train10 <- HoltWinters(Hawaii.Train10.ts, beta=TRUE, gamma=TRUE)
rmsetestHoltHawaii10 <- rmse(Hawaii.Test10$HIHAWA0URN, predict(Holt.Winters.Hawaii.Train10, n.ahead = 24))
rmsetestHoltHawaii10 # Result is 17.24666

Holt.Winters.Honolulu.Train10 <- HoltWinters(Honolulu.Train10.ts, beta=TRUE, gamma=TRUE)
rmsetestHoltHonolulu10 <- rmse(Honolulu.Test10$HIHONO7URN, predict(Holt.Winters.Honolulu.Train10, n.ahead = 24))
rmsetestHoltHonolulu10 # Result is 0.765775

Holt.Winters.LasVegas.Train10 <- HoltWinters(LasVegas.Train5.ts, beta=TRUE, gamma=TRUE)
rmsetestHoltLasVegas10 <- rmse(LasVegas.Test10$LASV832URN, predict(Holt.Winters.LasVegas.Train10, n.ahead = 24))
rmsetestHoltLasVegas10 # Result is 4.992916


#Arima for 5 years and modelling 1 year
Arima.Hawaii.Train5 <- forecast(auto.arima(Hawaii.Train5.ts), h = 12)
rmseTestARIMAHawaii5 <- rmse(Hawaii.Test5$HIHAWA0URN, Arima.Hawaii.Train5$mean)
rmseTestARIMAHawaii5 # Result is 1.164304

Arima.Honolulu.Train5 <- forecast(auto.arima(Honolulu.Train5.ts), h = 12)
rmseTestARIMAHonolulu5 <- rmse(Honolulu.Test5$HIHONO7URN, Arima.Honolulu.Train5$mean)
rmseTestARIMAHonolulu5 # Result is 0.6203774

Arima.LasVegas.Train5 <- forecast(auto.arima(Honolulu.Train5.ts), h = 12)
rmseTestARIMALasVegas5 <- rmse(LasVegas.Test5$LASV832URN, Arima.LasVegas.Train5$mean)
rmseTestARIMALasVegas5 # Result is 1.360905

#Arima for 10 years and forecasting 2 years
Arima.Hawaii.Train10 <- forecast(auto.arima(Hawaii.Train10.ts), h = 24)
rmseTestARIMAHawaii10 <- rmse(Hawaii.Test10$HIHAWA0URN, Arima.Hawaii.Train10$mean)
rmseTestARIMAHawaii10 # Result is 1.359419

Arima.Honolulu.Train10 <- forecast(auto.arima(Honolulu.Train10.ts), h = 24)
rmseTestARIMAHonolulu10 <- rmse(Honolulu.Test10$HIHONO7URN, Arima.Honolulu.Train10$mean)
rmseTestARIMAHonolulu10 # Result is 0.6138157

Arima.LasVegas.Train10 <- forecast(auto.arima(Honolulu.Train10.ts), h = 24)
rmseTestARIMALasVegas10 <- rmse(LasVegas.Test10$LASV832URN, Arima.LasVegas.Train10$mean)
rmseTestARIMALasVegas10 # Result is 0.9150728

#The Winners for the highest accuracy for the 10 years and 5 years as as followed:
#5 years: Hawaii:ARIMA   Las Vegas:SEs    Honolulu:ARIMA   
#10 YEARS: Hawaii:Naive   Las Vegas:ARIMA    Honolulu:SES

#Let's use the full time series to forecast using the best method
#ARIMA has the most correct entries followed by SES

#Lets create some training data
Hawaii.TrainFull.ts <- ts(HawaiiUnemploymentData$HIHAWA0URN, start = c(1990, 1), end = c(2019, 01), frequency =  12)
Honolulu.TrainFull.ts <- ts(HonoluluUnemploymentData$HIHONO7URN, start = c(1990, 1), end = c(2019, 01), frequency =  12)
LasVegas.TrainFull.ts <- ts(LasVegasUnemploymentData$LASV832URN, start = c(1990, 1), end = c(2019, 01), frequency =  12) 

#Use ARIMA to train 
Arima.Hawaii.Full <- forecast(auto.arima(Hawaii.TrainFull.ts), h = 60) #60 months is 5 years
Arima.Honolulu.Full <- forecast(auto.arima(Honolulu.TrainFull.ts), h = 60)
Arima.LasVegas.Full <- forecast(auto.arima(LasVegas.TrainFull.ts), h = 60)

#Plot ARIMA Model
autoplot(Arima.Hawaii.Full) +  xlab("Year") + ylab("Unemployment Rate") + ggtitle("ARIMA Forecasted Unemployment rate for Big Island")
autoplot(Arima.Honolulu.Full)  +  xlab("Year") + ylab("Unemployment Rate") + ggtitle("ARIMA Forecasted Unemployment rate for Honolulu")
autoplot(Arima.LasVegas.Full)  +  xlab("Year") + ylab("Unemployment Rate") + ggtitle("ARIMA Forecasted Unemployment rate for Las Vegas")

#SES
SES.Hawaii.TrainFull <- ses(Hawaii.TrainFull.ts, h = 60, alpha = 0.9, initial = "simple")
SES.Honolulu.TrainFull <- ses(Honolulu.TrainFull.ts, h = 60, alpha = 0.9, initial = "simple")
SES.LasVegas.TrainFull <- ses(LasVegas.TrainFull.ts, h = 60, alpha = 0.9, initial = "simple")

#SES Plotting
autoplot(SES.Hawaii.TrainFull) +  xlab("Year") + ylab("Unemployment Rate") + ggtitle("SES Forecasted Unemployment rate for Big Island")
autoplot(SES.Honolulu.TrainFull)  +  xlab("Year") + ylab("Unemployment Rate") + ggtitle("SES Forecasted Unemployment rate for Honolulu")
autoplot(SES.LasVegas.TrainFull)  +  xlab("Year") + ylab("Unemployment Rate") + ggtitle("SES Forecasted Unemployment rate for Las Vegas")



####################PART2##################################
RetailServicesResturants <- read_excel("MRTSSM7221USN.xls")
RetailFastFood <-read_excel("RSAFS.xls")
InternetUsers<-read_excel("ITNETUSERP2USA.xls")

#Lets plot them now
InflationPlot<- ggplot(data = monthlyInflationRate, aes(x=Month, y = T10YIE)) +
  geom_line(color = "#FF0027", size = 1.5) + stat_smooth(
    color = "#000000", fill = "#000000",
    method = "loess"
  ) + ggtitle("Inflation ") +
  xlab("Year 2003-2019") + ylab("Inflation Percent")
InflationPlot

InternetUSersPlot<- ggplot(data = InternetUsers, aes(x=InternetUsers$observation_date, y = InternetUsers$ITNETUSERP2USA)) +
  geom_line(color = "#FF0027", size = 1.5) + stat_smooth(
    color = "#000000", fill = "#000000",
    method = "loess"
  ) + ggtitle("Internet Users ") +
  xlab("Month and Year") + ylab("Internet Users per 100 people in US")
InternetUSersPlot

FastFoodPlot<- ggplot(data = RetailFastFood, aes(x=RetailFastFood$observation_date, y = RetailFastFood$RSAFS)) +
  geom_line(color = "#FF0027", size = 1.5) + stat_smooth(
    color = "#000000", fill = "#000000",
    method = "loess"
  ) + ggtitle("Amount spent on Fast Food") +
  xlab("Month and Year") + ylab("Monthly amount spent (in Millions of Dollars)")
FastFoodPlot

RestaurantPlot<- ggplot(data = RetailServicesResturants, aes(x=RetailServicesResturants$observation_date, y = RetailServicesResturants$MRTSSM7221USN)) +
  geom_line(color = "#FF0027", size = 1.0) + stat_smooth(
    color = "#000000", fill = "#000000",
    method = "loess"
  ) + ggtitle("Amount spent on Restaurant") +
  xlab("Month and Year") + ylab("Monthly amount spent (in Millions of Dollars)")
RestaurantPlot


InternetUsersLasVegas <- ggplot() + 
  geom_line(data = InternetUsers, aes(x=observation_date, y = ITNETUSERP2USA), color = "#207D5E", size = 1.0) +
  geom_line(data = LasVegasUnemploymentData, aes(x=observation_date, y = LasVegasUnemploymentData$LASV832URN), color = "#FF0027", size = 1.0) + ggtitle("Internet Rate vs Las Vegas Unemployment") +
  xlab("Month and Year") + ylab("Internet usage vs Las Vegas unemployment")
#Figure out how to divide Internet Users by 10
InternetUsersLasVegas



#Normalized the other data and scaled it to 1-14 and then ran CCF on it.
RetailFastFood$RSAFS <- normalize(RetailFastFood$RSAFS, method = "scale", range = c(1, 14), margin = 1L, on.constant =  "quiet")
RetailServicesResturants$MRTSSM7221USN <- normalize(RetailServicesResturants$MRTSSM7221USN, method = "scale", range = c(1, 14), margin = 1L, on.constant =  "quiet")
InternetUsers$ITNETUSERP2USA <- normalize(InternetUsers$ITNETUSERP2USA, method = "scale", range = c(1, 14), margin = 1L, on.constant =  "quiet")

ccfFastFoodHonolulu <- ccf(RetailFastFood$RSAFS, HonoluluUnemploymentData$HIHONO7URN, type = "correlation")
autoplot(ccfFastFoodHonolulu) + ggtitle("Fast Food and Honolulu Unemployment") +  ylab("Auto Correlation between the two variables")

ccfrestauranthonolulu <- ccf(RetailServicesResturants$MRTSSM7221USN, HonoluluUnemploymentData$HIHONO7URN)
autoplot(ccfrestauranthonolulu) + ggtitle("Restaurant Spendings and Honolulu Unemployment") +  ylab("Auto Correlation between the two variables")

ccfFastFoodHawaii <- ccf(RetailFastFood$RSAFS, HawaiiUnemploymentData$HIHAWA0URN)
autoplot(ccfFastFoodHawaii)+ ggtitle("Fast Food and Hawaii Unemployment") +  ylab("Auto Correlation between the two variables")

ccfRestaurantHawaii <- ccf(RetailServicesResturants$MRTSSM7221USN, HawaiiUnemploymentData$HIHAWA0URN)
autoplot(ccfRestaurantHawaii) + ggtitle("Restaurant and Hawaii Unemployment") +  ylab("Auto Correlation between the two variables")

ccfinternetusersHawaii <- ccf(InternetUsers$ITNETUSERP2USA, HawaiiUnemploymentData$HIHAWA0URN)
autoplot(ccfinternetusersHawaii) + ggtitle("Internet Users and Hawaii Unemployment") +  ylab("Auto Correlation between the two variables")

ccfinternetusersHonolulu <- ccf(InternetUsers$ITNETUSERP2USA, HonoluluUnemploymentData$HIHONO7URN)
autoplot(ccfinternetusersHonolulu) + ggtitle("Internet Users and Honolulu Unemployment") +  ylab("Auto Correlation between the two variables")

ccfrestaurantLV <- ccf(RetailServicesResturants$MRTSSM7221USN, LasVegasUnemploymentData$LASV832URN)
autoplot(ccfrestaurantLV) + ggtitle("Restaurant Spendings and Las Vegas Unemployment") +  ylab("Auto Correlation between the two variables")


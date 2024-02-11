###TIME SERIES FORECASTING


#Analytical Problem: To forecast the Sales (in thoushand units) of a Automobile company for the next 36 months

#PREPARING ENVIRONMENT
##DOWNLOADING PAPCKAGES
list.of.packages <- c("forecast", "ggplot2","MASS","caTools","sqldf","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(forecast) #package for arima forecasting
library(tseries)  #package for converting dataframe into a timeseries object
 
#Setting the working directory
path<-setwd("E:/IVY 2021-22/STAT + R SESSIONS/DATA/TSF")
getwd
data<-read.csv("Sales.csv",header = TRUE)
TSdata=data   #To create a backup of original data

#EXPLORING THE DATA
dim(TSdata) #We have 144 time series points (at a date level) and 2 vars(Date, Sales)
str(TSdata)
summary(TSdata)
colSums(is.na(TSdata)) #no missing values
names(TSdata)[c(1:2)]=c("Date","Sales") #renaming column headers
class(TSdata) #its a dataframe now, but need to convert it into a timeseries object so that ARIMA can work on it

#Transformation of the date data into time series
TSdata=ts(TSdata[,2],start=c(2003,1),frequency=12)

class(TSdata) #its a ts object now
start(TSdata) #Jan 2003
end(TSdata)   #Dec 2014
frequency(TSdata) #12 since its a monthly data
str(TSdata)
TSdata #ARIMA will work on this ts object

#plotting the sales
plot(TSdata,ylab="Sales", xlab="Year",main="Sales between 2003-2014",col="grey")
abline(reg = lm(TSdata~time(TSdata))) #adding a trend line and its a non stationary ts 
cycle(TSdata)
plot(aggregate(TSdata,FUN=mean))

##Data has both a trend and drift, i.e. time invariant mean and variance 
#Difference the data to remove trend and drift
plot(log10(TSdata),ylab="log(Sales)",xlab="Year",main="log(Sales) between 2003-2014",col="grey") #plotting the log of ts data

###Differencing the data to remove trend
plot(diff(TSdata,differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Sales) between 2003-2014",col="grey")
#The differenced data continues to have unequal variance 

#Differencing+Log transformation the data to remove trend and unequal variance
plot(diff(log10(TSdata),differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Log(Sales)) between 2003-2014",col="grey")
##So, with Log10 and 1 order of differencing makes the series stationary

#Checking the stationarity of transformed data using the Augmented Dickey-Fuller Test(ADF)

LDTSdata=diff(log10(TSdata),differences = 1) #new dataset which i get by differencing log10(TSdata) dataset
adf.test(LDTSdata,alternative="stationary")  #checking the stationary 
#at 90% confidence level the data has achieved stationary , Since, the p-value <0.05, hence, we reject the Ho: Series is Non-Stationary 

#Till here we took order of differencing 1 , so d = 1 (the one matrix of ARIMA).Now need to determine order of AR (p) and MA (q)

##creating the ACF and PACF plot (to determine p and q)
par(mfrow=c(1,2))
acf(diff(log10(TSdata)),main="ACF plot")    #ACF PLOT -- Moving Average or q
pacf(diff(log10(TSdata)),main="PACF plot")  #PACF PLOT -- Auto Regressive or p

#Running the ARIMA model with different combinations of p, d, q
ARIMAFit=arima((log10(TSdata)),c(0,1,1)) #p=0, d=1, q=1
ARIMAFit1=arima((log10(TSdata)),c(1,1,1)) #p=1, d=1, q=1
ARIMAFit2=arima((log10(TSdata)),c(0,1,0)) #p=0, d=1, q=1
ARIMAFit3=arima((log10(TSdata)),c(1,1,0)) #p=1, d=1, q=0

summary(ARIMAFit)
summary(ARIMAFit1)
summary(ARIMAFit2)
summary(ARIMAFit3)

#Running the ARIMA model-R, gives the best model fit by itself using package forecast
ARIMAFit1=auto.arima(log10(TSdata),approximation=TRUE,trace=TRUE)
summary(ARIMAFit1)

##did the model on log of the data but now need the output on actual series to that first predicting the output
pred=predict(ARIMAFit1,n.ahead=36)  #predicting future values 36 data points ahead
pred #predicted series for 2016 to 2017, this series is log series

##Ploting the observed data and forecasted data together
par(mfrow=c(1,1))
plot(TSdata,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab="Year",ylab="Sales") #showing the actual data till 2014
lines(10^(pred$pred),col="red") #adding the predicted part in the same graph by raising it to the power 10
#plotting the +-2 standard error to range of expected error in the same plot
plot(TSdata,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se),col="blue") #gives me the range of how much my fluctuation in my predicted series can be with +2, -2 sd
lines(10^(pred$pred-2*pred$se),col="black")

## then do the exponential since you had used log earlier.
normal_result=10^pred$pred ## you get in the format of original series the predicted values for January 2015 to DECEMBER 2017
normal_result

class(normal_result) #right now its a ts object
normal_result_df<-as.data.frame(normal_result) #converting it into a dataframe
normal_result_df_seq<-as.data.frame(seq(as.Date("2015/01/01"),as.Date("2017/12/01"), by="month")) #generating a sequence of date by month
library(dplyr)
final_result_df= cbind(normal_result_df_seq,normal_result_df)
colnames(final_result_df)<-c("Date","Sales_predicted") #renaming column headers
write.csv(final_result_df, "finalpredict.csv", row.names=FALSE)
plot(normal_result)





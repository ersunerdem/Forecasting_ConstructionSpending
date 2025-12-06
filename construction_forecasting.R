#Ersun Erdem Forecasting (ECON 138) Project
#   Forecasting U.S. Total Construction Spending

rm(list=ls()) #Clear working space
#INSTRUCTIONS:
#Edit the below working directory and file locations to match your setup, then run
#NOTE: You will want a folder called "graphs" in your working directory, as that's where all of the graphs
#      will output to...

#Set working directory, and load data
setwd("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper")
raw_data <- read.csv("data/TTLCON.csv")
raw_earnings_data <- read.csv("data/CES2000000008.csv")
raw_ppi_data <- read.csv("data/WPUSI012011.csv")


#Packages
library(fpp2)
library(fpp3)
library(tidyverse)
library(tsibble)
library(tseries)
library(dplyr)
library(vars)

#Helper functions
real_cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

real_cbrt2 <- function(x){
  x^(1/3)
}
#Back-transform forecasts
backtransform <- function(data, base_index=0, base){ #Base is a dataset, base_index is an index
  i = base_index
  if(base_index > length(base)-12){
    print("Shifted base")
    i = 0
  }
  #Need to inverse difference
  base1 <- real_cbrt(diff(base, lag=12))
  t_data <- diffinv(data, xi=base[(i+1)], lag=1)
  t_data2 <- t_data^3
  t_data3 <- diffinv(t_data2, xi=base1[(i+1):(i+12)], lag=12)
  return(t_data3)
}

#Back-transform forecasts
backtransform2 <- function(data, base_index=0, base){ #Base is a dataset, base_index is an index
  i = base_index
  if(base_index > length(base)-12){
    print("Shifted base")
    i = 0
  }
  #Need to inverse difference
  base1 <- log(base)
  t_data <- diffinv(data, xi=base[(i+1)], lag=1)
  t_data2 <- diffinv(t_data, xi=base1[(i+1):(i+12)], lag=12)
  t_data3 <- exp(t_data2)
  return(t_data3)
}

backtransform3 <- function(data, base_index=0, base){ #Base is a dataset, base_index is an index
  i = base_index
  if(base_index > length(base)-12){
    print("Shifted base")
    i = 0
  }
  #Need to inverse difference
  base1 <- log(base)
  base2 <- diff(log(base), lag=12)
  t_data <- diffinv(data, xi=base2[(i+1)], lag=1)
  t_data2 <- diffinv(t_data, xi=base1[(i+1):(i+12)], lag=12)
  t_data3 <- exp(t_data2)
  return(t_data3)
}

#Summary Statistics
summary(raw_data)


#Convert data to time series (only want the second column, since the first is date.)
const_ts <- ts(raw_data[,2]/1000000, start=c(1993,1),frequency=12)
earnings_ts <- ts(raw_earnings_data[,2], start=c(1993,1),frequency=12)
ppi_ts <- ts(raw_ppi_data[,2], start=c(1993,1),frequency=12)

#Show data with no log is still non-stationary
data_nolog <- diff(diff(const_ts,lag=12),lag=1)
ggAcf(data_nolog) + ggtitle("ACF for TTLCON with Twelfth and First Difference Taken")

#Plot
autoplot(const_ts) + ggtitle("Total Construction Spending in the United States (Trillions of Dollars)")
ggAcf(const_ts) + ggtitle("ACF for TTLCON")
ggPacf(const_ts) + ggtitle("PACF for TTLCON")
ggseasonplot(const_ts) + ggtitle("Seasonality Plot for TTLCON")
checkresiduals(const_ts) + ggtitle("Residual Check for TTLCON")

#Difference the data
const_diff <- diff(const_ts)
ggAcf(const_diff)

const_diff2 <- diff(const_ts, lag=2)
ggAcf(const_diff2)

const_seasonaldiff <- diff(const_ts, lag=12)
ggAcf(const_seasonaldiff)
ggPacf(const_seasonaldiff)


#stationary_const <- diff(log(diff(const_ts, lag=12))) #OLD
#stationary_const <- diff(real_cbrt(diff(const_ts, lag=12)))
stationary_const <- diff(diff(log(const_ts), lag=12))
print(sum(is.na(stationary_const)))

ggAcf(stationary_const) + ggtitle("ACF for TTLCON Logged, then with Twelfth Difference, then First Difference Taken")


autoplot(stationary_const)

ggAcf(stationary_const)
ggPacf(stationary_const)
checkresiduals(stationary_const)

#Stationarity Test by Augmented Dickey-Fuller, p-value = 0.01, can reject null hypothesis of non-stationary data
adf_result <- adf.test(stationary_const)
print(adf_result)


data <- stationary_const

#Check ppi, earnings data
#checkresiduals(ppi_ts)
#checkresiduals(earnings_ts)

#Below is absolutely absurd... if I don't really care about forecasts of indicators,
#   Just keep indicators non-stationary.
#   Non-stationarity of the indicators SHOULDN'T matter in this case...
#stationary_ppi <- diff(diff(diff(diff(real_cbrt(diff(ppi_ts, lag=12))), lag=2), lag=2), lag=2)
#checkresiduals(stationary_ppi)
#pacf(stationary_ppi)
#Box.test(stationary_ppi, type="Ljung-Box")

#df <- as.data.frame(cbind(data, window(ppi_ts, start=start(data)), window(earnings_ts, start=start(data))))


#===============================================================================
#=========================  ANALYSIS SECTION    ================================
#===============================================================================


#Split data into test, train
train.data <- window(data, end=c(2020,6))
test.data <- window(data, start=c(2020,7))
train.orig.data <- window(const_ts, end=c(2020,6))
test.orig.data <- window(const_ts, start=c(2020,5))

#Do same split for dataframe for VAR
#train.df <- df[1:(nrow(df)-length(test.data)),]
#test.df <- df[(nrow(df)-length(test.data)+1):nrow(df),]

#Check VARs
#VARselect(df, lag.max=5,
#          type="const")[["selection"]]
#var1 <- VAR(df, p=1, type="const")
#serial.test(var1, lags.pt=5, type="PT.asymptotic")
#var3 <- VAR(df, p=3, type="const")
#serial.test(var3, lags.pt=5, type="PT.asymptotic")



transform_data <- backtransform3(train.data, base=const_ts)
transform_data2 <- tail(backtransform3(test.data, base=const_ts, base_index=length(const_ts)-length(test.data)-13),-12)


#Plot original data vs transform_data
autoplot(const_ts) + ggtitle("Transform and Backtransform Original Data to Verify Mapping") +
  autolayer(transform_data, series="Training Data") +
  autolayer(transform_data2, series="Test Data") #VERIFIED: It worked

#NOW, create forecasts (on training data, evaluate on test data)
fc.naive <- naive(train.orig.data, h=length(test.data))
fc.snaive <- snaive(train.orig.data, h=length(test.data))
ann_fit <- nnetar(train.data, P=0)
fc.ann <- forecast(ann_fit,h=length(test.data), times=800000)
arima.model <- auto.arima(train.orig.data) #Run ARIMA on original, untransformed data
summary(arima.model)
fc.arima <- forecast(arima.model, h=length(test.data))
arima.manual <- Arima(train.orig.data, order = c(4, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
summary(arima.manual)
fc.arima.manual <- forecast(arima.manual, h=length(test.data))
tryCatch({
  autoplot(window(data, start=c(2010,1)))+
    #autolayer(fc.naive, series="Naïve", PI=FALSE)+
    #autolayer(fc.snaive, series="Snaïve", PI=FALSE)+
    autolayer(fc.ann, series="ANN", PI=FALSE)+
    #autolayer(fc.arima, series="ARIMA(0,0,0)",PI=FALSE) +
    #autolayer(fc.arima.manual, series="ARIMA(4,1,1)(0,1,1)[12]", PI=FALSE)+
    #autolayer(test.data, series="TEST DATA")+
    ggtitle("Forecasts for Total Construction Spending from Training Set (transformed data)") +
    xlab("Year") + ylab("Trillions of Dollars of spending, Transformed") +
    guides(colour=guide_legend(title="Forecast")) +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ALL_Test_Data_vs_Test_Data.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})



#Backtransform the forecasts
#fc.naive.bt <- backtransform(fc.naive$mean, base=const_ts[length(const_ts)-length(fc.naive$mean)+1:length(const_ts)])
#fc.snaive.bt <- backtransform(fc.snaive$mean, base=const_ts[length(const_ts)-length(fc.snaive$mean)+1:length(const_ts)])

#fc.naive.bt <- tail(backtransform(fc.naive$mean, base=const_ts, base_index=length(data)-length(test.data)-13),-12)
#fc.snaive.bt <- tail(backtransform(fc.snaive$mean, base=const_ts, base_index=length(data)-length(test.data)-13),-12)
fc.ann.bt <- tail(backtransform(fc.ann$mean, base=const_ts, base_index=length(data)-length(test.data)-13),-12)
test.data.bt <- tail(backtransform(test.data, base=const_ts, base_index=length(data)-length(test.data)-13),-12)
#fc.arima.bt <- tail(backtransform(fc.arima$mean, base=const_ts, base_index=length(data)-length(test.data)-13),-12)
#fc.arima.manual.bt <-tail(backtransform(fc.arima.manual$mean, base=const_ts, base_index=length(data)-length(test.data)-13),-12)

#Plot the forecasts
tryCatch({
  autoplot(window(const_ts, start=c(2020,5))) +
    #autolayer(backtransform(tail(data, length(fc.naive$mean)), base=const_ts, base_index=length(data)-length(fc.naive$mean)), series="Transform/Backtransform Data") +
    #autolayer(fc.arima, series="ARIMA(0,1,3)(1,1,1)[12]", PI=FALSE) +
    autolayer(fc.arima.manual, series="ARIMA(4,1,1)(0,1,1)[12]", PI=FALSE) +
    autolayer(fc.naive,
              series="Naïve", PI=FALSE) +
    autolayer(fc.snaive,
              series="Snaïve", PI=FALSE) +
    autolayer(fc.ann.bt, series="ANN") +
    #autolayer(backtransform(train.data, base=const_ts))+
    ggtitle("Forecasts for Total Construction Spending from Training Set") +
    xlab("Year") + ylab("Trillions of Dollars ($)") +
    guides(colour=guide_legend(title="Forecast")) +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ALL_Test_Data_vs_Test_Data_BT.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
tryCatch({
  #autoplot(window(const_ts, start=c(2020,5))) +
  autoplot(window(test.orig.data, start=c(2020,5))) +
    #autolayer(backtransform(tail(data, length(fc.naive$mean)), base=const_ts, base_index=length(data)-length(fc.naive$mean)), series="Transform/Backtransform Data") +
    autolayer(fc.naive,
              series="Naïve") +
    autolayer(fc.snaive,
              series="Snaïve") +
    #autolayer(fc.ann.bt, series="ANN") +
    autolayer(fc.arima, series="ARIMA(0,1,3)(1,1,1)[12]", PI=FALSE) +
    autolayer(fc.arima.manual, series="ARIMA(4,1,1)(0,1,1)[12]",PI=FALSE) +
    #autolayer(backtransform(train.data, base=const_ts))+
    ggtitle("Forecasts for Total Construction Spending from Training Set, Excl. ANN") +
    xlab("Year") + ylab("Trillions of Dollars ($)") +
    guides(colour=guide_legend(title="Forecast")) +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ALL_Test_Data_vs_Test_Data_noANN.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})



acc.naive <- accuracy(fc.naive, test.orig.data)
acc.snaive <- accuracy(fc.snaive, test.orig.data)
acc.ann <- accuracy(fc.ann.bt, test.orig.data)
acc.auto.arima <- accuracy(fc.arima, test.orig.data)
acc.manual.arima <- accuracy(fc.arima.manual, test.orig.data)

acc.naive
acc.snaive
acc.ann
acc.auto.arima
acc.manual.arima
tryCatch({
  sink("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/accuracy_results.txt")
  print(paste("MODEL: ", " RMSE, MAE"))
  print(paste("Naive: ", acc.naive[2,2], acc.naive[2,3]))
  print(paste("Snaive: ", acc.snaive[2,2], acc.snaive[2,3]))
  print(paste("ARIMA(4,1,1)(0,1,1)[12]: ", acc.manual.arima[2,2], acc.manual.arima[2,3]))
  print(paste("ANN: ", acc.ann[1,2], acc.ann[1,3]))
  #print(paste("ARIMA(0,1,3)(1,1,1)[12]: ", acc.auto.arima[2,2], acc.auto.arima[2,3]))
  sink()
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
#Now, actually forecast the data
#Want to show PI's this time

ann_fit2 <- nnetar(data, P=0)
fc.ann.full <- forecast(ann_fit2,h=length(test.data), times=800000, PI=TRUE)
fc.naive2 <- naive(const_ts, h=length(test.data))
fc.snaive2 <- snaive(const_ts, h=length(test.data))
auto.arima2 <- auto.arima(const_ts)
summary(auto.arima2)
fc.arima2 <- forecast(auto.arima2, h=length(test.data))

tryCatch({
  autoplot(data) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    autolayer(fc.naive2) +
    labs(x = "Year", y = "Log Trillions of Dollars, Differenced Twice ($)", title = "Forecasts for Total Construction Spending (Transformed Data), Naïve") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Naive_ForecastOfTransformedData.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})


tryCatch({
  autoplot(data) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    autolayer(fc.snaive2) +
    labs(x = "Year", y = "Log Trillions of Dollars, Differenced Twice ($)", title = "Forecasts for Total Construction Spending (Transformed Data), Snaïve") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Snaive_ForecastOfTransformedData.jpg")
  
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})

tryCatch({
  autoplot(data) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    autolayer(fc.ann.full) +
    labs(x = "Year", y = "Log Trillions of Dollars, Differenced Twice ($)", title = "Forecasts for Total Construction Spending (Transformed Data), ANN") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/ANN_ForecastOfTransformedData.jpg")
  
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})

tryCatch({
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    autolayer(fc.arima2) +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending (Transformed Data), ARIMA(4,1,1)(0,1,1)[12]") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/ARIMA_ForecastOfTransformedData.jpg")
  
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})


#Reconstruct Confidence Intervals with Back-Transformations
fc.ann.full.bt.mean <- tail(backtransform3(fc.ann.full$mean, base=const_ts, base_index=length(const_ts)-13),-12)
fc.ann.full.bt.lwr_10 <- tail(backtransform3(fc.ann.full$lower[,1], base=const_ts, base_index=length(const_ts)-13),-12)
fc.ann.full.bt.lwr_2.5 <- tail(backtransform3(fc.ann.full$lower[,2], base=const_ts, base_index=length(const_ts)-13),-12)
fc.ann.full.bt.upr_90 <- tail(backtransform3(fc.ann.full$upper[,1], base=const_ts, base_index=length(const_ts)-13),-12)
fc.ann.full.bt.upr_97.5 <- tail(backtransform3(fc.ann.full$upper[,2], base=const_ts, base_index=length(const_ts)-13),-12)

fc.ann.full.bt <- fc.ann.full
fc.ann.full.bt$mean <- fc.ann.full.bt.mean
fc.ann.full.bt$lower <- as.matrix(data.frame(`90%`=fc.ann.full.bt.lwr_10,`97.5%`=fc.ann.full.bt.lwr_2.5), ncol=2)
fc.ann.full.bt$upper <- as.matrix(data.frame(`10%`=fc.ann.full.bt.upr_90,`2.5%`=fc.ann.full.bt.upr_97.5), ncol=2)


testdata.bt <- tail(backtransform3(test.data, base=const_ts, base_index=length(const_ts)-length(test.data)-13),-12)
#Check Back-Transform Again
autoplot(const_ts) + autolayer(transform_data, series="Back-Transformed Data") +
  autolayer(testdata.bt, series="Back-Transformed Test Data") + ylab("Trillions of Dollars ($)")
tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    #autolayer(fc.ann.full.bt, series="ANN") +
    autolayer(fc.naive2, series="Naïve", PI=FALSE) +
    #autolayer(fc.snaive2, series="Snaïve") +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, Naïve without PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_Naive_noPI.jpg") 
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    #autolayer(fc.ann.full.bt, series="ANN") +
    #autolayer(fc.naive2, series="Naïve") +
    autolayer(fc.snaive2, series="Snaïve", PI=FALSE) +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, Snaïve without PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_Snaive_noPI.jpg")
  
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    #autolayer(fc.ann.full.bt, series="ANN") +
    autolayer(fc.naive2, series="Naïve") +
    #autolayer(fc.snaive2, series="Snaïve") +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, Naïve with PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_NaiveOnly.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    #autolayer(fc.ann.full.bt, series="ANN") +
    #autolayer(fc.naive2, series="Naïve") +
    autolayer(fc.snaive2, series="Snaïve") +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, Snaïve with PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_SnaiveOnly.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    #autolayer(fc.ann.full.bt, series="ANN") +
    autolayer(fc.naive2, series="Naïve") +
    autolayer(fc.snaive2, series="Snaïve") +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, Naïve and Snaïve") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_NaiveAndSnaive.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    #autolayer(fc.ann.full.bt, series="ANN") +
    autolayer(fc.naive2, series="Naïve", PI=FALSE) +
    autolayer(fc.snaive2, series="Snaïve", PI=FALSE) +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, Naïve and Snaïve, without PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_NaiveAndSnaive_noPI.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})

tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    autolayer(fc.ann.full.bt, series="ANN") +
    #autolayer(fc.naive2, series="Naïve") +
    #autolayer(fc.snaive2, series="Snaïve") +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, ANN") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ANN.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})

tryCatch({
  # Code that might produce an error
  autoplot(data) +
    #scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    autolayer(fc.ann.full, series="ANN") +
    #autolayer(fc.naive2, series="Naïve") +
    #autolayer(fc.snaive2, series="Snaïve") +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Log Trillions of Dollars, Differenced Twice ($)", title = "Forecasts for Total Construction Spending, ANN - Transformed Data") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ANN_transformed.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})

tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    autolayer(fc.ann.full.bt, series="ANN",PI=FALSE) +
    autolayer(fc.naive2, series="Naïve",PI=FALSE) +
    autolayer(fc.snaive2, series="Snaïve",PI=FALSE) +
    autolayer(fc.arima2, series="ARIMA(4,1,1)(0,1,1)[12]", PI=FALSE) +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, All Models, without Prediction Intervals") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ALL_noPI.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})

tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    #autolayer(fc.ann.full.bt, series="ANN") +
    #autolayer(fc.naive2, series="Naïve") +
    #autolayer(fc.snaive2, series="Snaïve") +
    autolayer(fc.arima2, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, ARIMA(4,1,1)(0,1,1)[12]") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ARIMA.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})

tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    #autolayer(fc.ann.full.bt, series="ANN") +
    #autolayer(fc.naive2, series="Naïve") +
    #autolayer(fc.snaive2, series="Snaïve") +
    autolayer(fc.arima2, series="ARIMA(4,1,1)(0,1,1)[12]", PI=FALSE) +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecast for Total Construction Spending, ARIMA(4,1,1)(0,1,1)[12], without PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ARIMA_noPI.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    autolayer(fc.ann.full.bt, series="ANN", PI=FALSE) +
    #autolayer(fc.naive2, series="Naïve") +
    #autolayer(fc.snaive2, series="Snaïve") +
    #autolayer(fc.arima2.bt, series="ARIMA(4,1,1)(0,1,1)[12]", PI=FALSE) +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecast for Total Construction Spending, ANN, without PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ANN_NoPI.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})
tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    autolayer(fc.ann.full.bt, series="ANN") +
    autolayer(fc.naive2, series="Naïve") +
    autolayer(fc.snaive2, series="Snaïve") +
    autolayer(fc.arima2, series="ARIMA(4,1,1)(0,1,1)[12]") +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, All Models, with PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ALL_withPI.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})

tryCatch({
  # Code that might produce an error
  autoplot(const_ts) +
    scale_x_continuous(limits = c(c(2020), NA)) +
    #autolayer(data.bt, series="Backtransformed Data") +
    autolayer(fc.ann.full.bt, series="ANN", PI=FALSE) +
    autolayer(fc.arima2, series="ARIMA(4,1,1)(0,1,1)[12]", PI=FALSE) +
    autolayer(fc.naive2, series="Naïve", PI=FALSE) +
    autolayer(fc.snaive2, series="Snaïve", PI=FALSE) +
    labs(x = "Year", y = "Trillions of Dollars ($)", title = "Forecasts for Total Construction Spending, All Models, without PI") +
    ggsave("C:/Users/ersun/Documents/From USB/School/SJSU/ECON 138 - Economic Forecasting/Forecasting Paper/graphs/Forecasts_ALL_NoPI.jpg")
}, error = function(e) {
  # This empty function effectively ignores the error
  NULL 
})





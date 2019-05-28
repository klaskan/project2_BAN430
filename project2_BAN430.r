
###########################
#        PART 1           #
###########################
library(fpp2)
library(GGally)
library(forecast)
library(ggplot2)
library(rdatamarket)

#=============Setup==============
#Wipe console
cat("\014")
#Clean environment
rm(list=ls())
#Set wd
setwd("/Users/klaskan/Desktop/Exam2")
#Importing Quarterly Australian private final consumption expenditure as ts
auscons <- as.ts(dmseries("http://bit.ly/1ONlQzK"))

#========Training & test set==========
train_auscons <- subset(auscons, end=length(auscons)-12) #Training set
test_auscons <- subset(auscons, start=length(auscons)-11) #Test set. Length = 12, counts 
length(test_auscons)

#=========Exploratory data analysis==============

#Plotting the data
autoplot(train_auscons, main = "The Quarterly Australian private final consumption expenditure") +
  geom_smooth()

#Summary statistics
summary(train_auscons)

#Plot that clearly shows seasonality
seasonplot_auscons <- ggseasonplot(train_auscons, year.labels=FALSE, continuous=TRUE)
plot(seasonplot_auscons)

#Decompose data
STL_train_auscons <- stl(train_auscons, s.window="periodic", robust=TRUE)
autoplot(STL_train_auscons, main="STL train_auscons")

#Lag plot
gglagplot(train_auscons)


#Correlogram / ACF plot 
ggAcf(train_auscons)
ggPacf(train_auscons)

#Ljung box test
Box.test(train_auscons, lag=24)

#Decompose data
STL_train_auscons <- stl(train_auscons, s.window="periodic", robust=TRUE, allow.multiplicative.trend=TRUE)
autoplot(STL_train_auscons, main="STL train_auscons")

#===========Estimating the SARIMA==========
#checking or non-stationary data
tsdisplay(train_auscons)

#Differencing data with lag 4
stationary_train_auscons <- diff(train_auscons, lag=4)
tsdisplay(stationary_train_auscons)

#Constructing the SARIMA model
estimated_ARIMA <- Arima(train_auscons, order=c(1,0,0), seasonal=c(0,1,1))
estimated_ARIMA


#We end by summarizing the residuals
checkresiduals(estimated_ARIMA)


#============auto.Arima modelleing==============
#Fiting two arima models
nostepwise_auto_ARIMA <- auto.arima(train_auscons,stepwise=FALSE, approximation=FALSE)

#Showing ARIMA results
nostepwise_auto_ARIMA

#Checking residuals
checkresiduals(nostepwise_auto_ARIMA)

#Finding the best fit on our test data
accuracy(forecast(nostepwise_auto_ARIMA), test_auscons)
accuracy(forecast(estimated_ARIMA), test_auscons)

#We plot the most accurate (stepwise_arima_auscons)
autoplot(forecast(nostepwise_auto_ARIMA, h=12)) +
  autolayer(test_auscons, series = "Test data") 




#========Plotting the ARIMA model==========

#Plot full estimated arima plot
autoplot(forecast(estimated_ARIMA, h=12), xlab = "Time", ylab = "Private final consumption expenditure" ) +
  autolayer(test_auscons, series = "Test data") 

#Plot estimated arima model from 1987 - 1992
autoplot(forecast(estimated_ARIMA, h=12), xlim=c(1987,1992), xlab = "Time", ylab = "Private final consumption expenditure") +
  autolayer(test_auscons, series = "Test data") 



#Checking residuals
checkresiduals(stepwise_arima_auscons2)

tsdisplay(residuals(stepwise_arima_auscons2), lag.max=45)


test <- ARMAacf(ar=0.7634, 1.0, lag.max = 12)

plot(test)


###########################
#        PART 2           #
###########################
library(fpp2)
library(GGally)
library(forecast)
library(ggplot2)
library(rdatamarket)
library(regress)
library(ForecastComb)
#=========setup==========
#Wipe console
cat("\014")
#Clean environment
rm(list=ls())
#Set wd
setwd("/Users/klaskan/Desktop/Exam2")
#Importing lynx data as ts
lynx <- as.ts(dmseries("http://bit.ly/10Scgaz"))

#=======Training and test set==========
train_lynx <- subset(lynx, end=length(lynx)-12) #Training set
test_lynx <- subset(lynx, start=length(lynx)-11) #Test set. Length = 12, counts 
length(test_auscons)

train_lynx2 <- subset(lynx, end=length(lynx)-24) #Training set
test_lynx2 <- subset(lynx, start=length(lynx)-23) #Test set. Length = 12, counts
length(test_lynx2)
#========Exploratory data analysis=========

#Plotting the time series.
autoplot(lynx, xlab = "Time", ylab = "Lynx")

#ACF to see how long the cyclic pattern is + PACF
tsdisplay(lynx)

log_lynx <- log(train_lynx)
tsdisplay(log_lynx)

ggAcf(log_lynx, type = "partial")

#=========Make the data stationary==========


#==========Estimated lynx ARIMA===============
estimated_lynx_ARIMA1 <- arima(log_lynx, order=c(2,0,0))
estimated_lynx_ARIMA2<- arima(log_lynx, order=c(1,0,0))
estimated_lynx_ARIMA3<- arima(log_lynx, order=c(3,0,0))
estimated_lynx_ARIMA4<- arima(log_lynx, order=c(4,0,0))
estimated_lynx_ARIMA5<- arima(log_lynx, order=c(5,0,0))
estimated_lynx_ARIMA6<- arima(log_lynx, order=c(6,0,0))
#Findinf smalles AIC
estimated_lynx_ARIMA1$aic
estimated_lynx_ARIMA2$aic
estimated_lynx_ARIMA3$aic
estimated_lynx_ARIMA4$aic #WinneR!
estimated_lynx_ARIMA5$aic
estimated_lynx_ARIMA6$aic
#================auto.arima==================

#Fitting the arima model
ARIMA_lynx <- auto.arima(train_lynx, stepwise=FALSE, approximation=FALSE)
ARIMA_lynx

#check accuracy of the ARIMA model 
accuracy(forecast(ARIMA_lynx), test_lynx)
accuracy(forecast(estimated_lynx_ARIMA), test_lynx )

#Plotting forecast with autolayer test data 
autoplot(forecast(ARIMA_lynx, h=12)) +
  autolayer(test_lynx, series = "Test data")


#==================Neural network autoregressive model===================
#Neural network autoregressive model.lambda = "auto"
NNfit <- nnetar(train_lynx, lambda = "auto")
fc_NNfit <- forecast(NNfit, PI=TRUE, h=12)
fc_NNfit

#Plotting the NN
autoplot(fc_NNfit) +
  autolayer(test_lynx, series = "Test data")

#Checing accurasy of the NNAM
accuracy(fc_NNfit, lynx)



#=========Smaller traing set, bigger test set=========

ARIMA_lynx2 <- auto.arima(train_lynx2, stepwise=FALSE, approximation=FALSE)
ARIMA_lynx2_fc <- forecast(ARIMA_lynx2, h=24)
NNfit2 <- nnetar(train_lynx2, lambda = 0)
NNfit2_fc <- forecast(NNfit2)

accuracy(ARIMA_lynx2_fc, test_lynx2)
accuracy(forecast(NNfit2) , test_lynx2)

autoplot(forecast(NNfit2, h=24)) +
  autolayer(test_lynx2, series = "Test data")

autoplot(forecast(ARIMA_lynx2_fc, h=24)) +
  autolayer(test_lynx2, series = "Test data")

#===============Combining the forecasts===================

ARIMA_forecast <- forecast(ARIMA_lynx)
NN_forecast <- forecast(NNfit)
#making a combined model
combination_average <- (ARIMA_forecast[["mean"]] + NN_forecast[["mean"]])/2
#Checing the models accuracy
accuracy(combination_average, lynx)


#Plotting combined fc 
autoplot(lynx) + 
  autolayer(NN_forecast, series="ARNN") + 
  autolayer(ARIMA_forecast, series="ARIMA", PI=FALSE)+
  autolayer(combination_average, series="combination")

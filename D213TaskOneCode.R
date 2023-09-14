#############################################
# This file is a time series analysis of the hospital's daily revenue data for D213 at WGU.
# I will use this data to forecast the next month of daily revenue.
# Created by Lex Matthews.
#############################################

# Clear all variables in workspace.
rm(list=ls())

# install the necessary libraries and packages
install.packages("readr")
library("readr")
install.packages("dplyr")
library("dplyr")
install.packages("zoo")
library("zoo")
install.packages("tseries")
library("tseries")
install.packages("forecast")
library("forecast")
install.packages("xts")
library("xts")

# import the data
medical_time_series_ <- read_csv("medical_time_series.csv")

# declare the data as time series data
medical_time_series_0 <- ts(medical_time_series_$Revenue, start = min(medical_time_series_$Day), end = max(medical_time_series_$Day))

################################################
# Preliminary Analysis
################################################

#visualize
plot.ts(medical_time_series_0, xlab = "Day", ylab = "Revenue($M)", main = "Hospital Revenue per Day" )
tsdisplay(medical_time_series_0)
# possible seasonality, revenue appears to dip approximately every three months

#augmented dickey-fuller test for stationarity
adf.test(medical_time_series_0) 
# p value greater than .05, series has a trend

# make the series stationary by taking the difference
diff <- diff(medical_time_series_0)
plot(diff, main = "First Difference of Revenue Time Series")

adf.test(diff)
# p-value smaller than .05, meaning the data is stationary

# run autoarima to indicate if there is seasonality and to find p,d,q
diff_arima <- auto.arima(diff, ic = "aic", trace = TRUE)
# parameters p,d,q are 1,0,0
# lack of any other numbers indicates there is no seasonality
# AIC is 879.98

# spectral density
spectrum <- spectrum(diff)
# the frequency is 0.000385 which means the cycle is the inverse (2597.4)
# this is longer than the data, indicating there is no seasonality

checkresiduals(diff)
# looks how we expect it to

#######################################
# Now that the data is stationary, we can create the model.
#######################################

# split data into training and test data
train <- head(diff, length(diff)-30)
test <- tail(diff, 30)
# the training data is everything up to the last 30 days
# test data is the last 30 days of data

#building arima model
medicalmodel=arima(train, order=c(1,0,0))
summary(medicalmodel)

##################
# diagnostic check
##################
acf(residuals(medicalmodel))
plot.ts(residuals(medicalmodel))
gghistogram(residuals(medicalmodel))
checkresiduals(medicalmodel)

#################
# forecasting
#################

fcast <- predict(medicalmodel, n.ahead=30)
plot.ts(test, xlab = "Day", ylab = "Revenue ($M)", main = "Test Data and Prediction")
lines(fcast$pred, col="blue")
legend(700, -0.5, legend=c("Test Data", "Prediction"), 
       fill = c("black", "blue"))

#################
# measure accuracy
#################

fmean <- mean(fcast$pred) # 0.05245843
testmean <- mean(test) # -0.05893368

# calculate the mean squared error
mse.df <- data.frame(pred = fcast$pred, actual = test)
mean((mse.df$actual - mse.df$pred)^2)

#MAE 
mean(abs(fmean- testmean))
#MAPE 
100*mean(abs(fmean - testmean)) 

#################
# future predictions
#################
futurepred <- forecast(medicalmodel, h = 120)
plot(futurepred, xlim = c(730,820), xlab = "Revenue ($M)", ylab = "Day")


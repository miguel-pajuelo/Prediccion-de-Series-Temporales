#################################################################################
##########################       Lab: SARIMA      ##############################
#################################################################################
library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(tidyverse)

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("ARIMA_07.dat", header=TRUE)
head(fdata)

# Convert to time series object
y <- ts(fdata$y)
y
# for daily data
autoplot(y)


## Training and validation ------------------------------------------------------------
y.TR <- subset(y, end = 1000)
y.TV <- subset(y, start = 1001)

## Identification and fitting frocess -------------------------------------------------------------------------------------------------------
autoplot(y.TR, color = "orange") + 
  autolayer(y.TV, color = "blue")

# Box-Cox transformation
Lambda <- BoxCox.lambda.plot(y.TR, window.width = 12)

# Lambda <- BoxCox.lambda(y) #other option
z <- BoxCox(y.TR,Lambda)

par(mfrow = c(2, 1))
plot(y) 
plot(z)

# Differentiation: if the ACF decreases very slowly -> needs differenciation
ggtsdisplay(z,lag.max = 100)

# Regular Differentiation
Bz <- diff(z,differences = 1)
ggtsdisplay(Bz, lag.max = 4*12) #differences contains the order of differentiation

# Seasonal Differentiation
B12Bz <- diff(Bz,differences = 1, lag = 12)
ggtsdisplay(B12Bz, lag.max = 4*12) #differences contains the order of differentiation

# Fit seasonal model with estimated order
arima.fit <- Arima(y.TR,
                   order=c(2, 1,1),
                   lambda = Lambda,
                   seasonal = list(order=c(0,1,0), period=12),
                   include.constant = FALSE)

summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lag=100)

# Check fitted
autoplot(y.TR, series = "Real")+
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


# Perform future forecast
y_est <- forecast(arima.fit, h=12)
autoplot(y_est)

## Validation error for h = 1 -------------------------------------------------------------------------------------------------------
# Obtain the forecast in validation for horizon = 1 using the trained parameters of the model
y.TV.est <- y * NA

for (i in seq(length(y.TR) + 1, length(y), 1)){# loop for validation period
  y.TV.est[i] <- forecast(subset(y, end=i-1), # y series up to sample i 
                          model = arima.fit,       # Model trained (Also valid for exponential smoothing models)
                          h=1)$mean                # h is the forecast horizon
}

head(y.TV.est, 48)
na.omit(y.TV.est)

#Plot series and forecast 
autoplot(y) +
  forecast::autolayer(y.TV.est)

#Compute validation errors
accuracy(y.TV.est, y)




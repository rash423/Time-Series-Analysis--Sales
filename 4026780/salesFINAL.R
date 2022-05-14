install.packages("seasonal")
install.packages("fpp2")
install.packages("Metrics")
library(tidyverse)
library(seasonal) 
library(fpp2)
library(Metrics)
library(forecast)

# Loading the data set
sales_dt <- read.csv("salesFinal.csv")
head(sales_dt)
sales_dt$month = NULL
sales_dt

#--------------------------           Naive         ----------------------------------------------------------------------------------
#naive = snaive(data, h=length(validation))
naive = snaive(sales_dt, h=12)
naive

# MAPE(naive$mean, validation) * 100

plot(sales_dt$sales, col="blue", xlab="Time Points(Months)", ylab="Sales", main="Seasonal Naive Forecast", type='l')
lines(naive$mean, col="red", lwd=2)



#-----------------------------        Exponential Smoothing (multiplicative)     --------------------------------------------
ets_model = ets(sales_dt$sales, allow.multiplicative.trend = TRUE)
summary(ets_model)

ets_forecast = forecast(ets_model, h=12)
ets_forecast
# MAPE(ets_forecast$mean, validation) *100

#-----------------------------       Exponential Smoothing (additive)         --------------------------------------------------
ets_model = ets(sales_dt$sales, allow.multiplicative.trend = FALSE)
summary(ets_model)

ets_forecast = forecast(ets_model, h=12)
ets_forecast
# MAPE(ets_forecast$mean, validation) *100


#----------------------------------             ARIMA                   ------------------------------------------------------------------------
arima_optimal = auto.arima(sales_dt$sales)
arima_optimal
summary(arima_optimal)
arima_forecast = forecast(arima_optimal, h=12)
arima_forecast

#-------------------------        Holt Winters           --------------------------------------------------------
hw_model = HoltWinters(sales_dt, seasonal = "additive")

#------------------------------------           Double Seasonal Holt winters   --------------------------------
dshw_model = dshw(sales_dt$sales, period1=3, period2 = 4, h=12)
#MAPE(dshw_model$mean, validation)*100

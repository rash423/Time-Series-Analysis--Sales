install.packages("seasonal")
install.packages("fpp2")
install.packages("Metrics")
library(tidyverse)
library(seasonal) 
library(fpp2)
library(Metrics)

# Loading the data set
sales_dt <- read.csv("salesFinal.csv")
head(sales_dt)

# We can tell that this is monthly data and that it begins in 2020. 
#Let's turn the value column into a time series object with the function ts() and then we will print it out.
sales_ts <- ts(sales_dt$sales)
sales_ts

sales <- ts(sales_dt$sales, frequency = 7, start = 2020)
sales
sales_dt$month = NULL
sales_dt
model = auto.arima(sales_dt)
model
# model summary
summary(model)

# forecasting
forecast = predict(model,12)
forecast
# fit <- auto.arima(sales_ts)

model1<- hw(sales_dt, seasonal = "multiplicative", PI=FALSE)
model1
plot(model1)


model2<-hw(sales_dt, seasonal = "additive", PI=FALSE)
model2
plot(model2)

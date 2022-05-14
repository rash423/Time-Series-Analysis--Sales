library(forecast)
library(MLmetrics)
data <-c(74104961.74,120441361.99, 184463589.30, 129305904.74, 45479670.96, 53376327.03, 126183313.81)
class(data)

#naive = snaive(data, h=length(validation))
naive = snaive(data, h=12)
naive

# MAPE(naive$mean, validation) * 100

plot(data, col="blue", xlab="Time Point", ylab="Sales", main="Seasonal Naive Forecast", type='l')
lines(naive$mean, col="red", lwd=2)

####################################### Exponential Smoothing (multiplicative) ############################################
ets_model = ets(data, allow.multiplicative.trend = TRUE)
summary(ets_model)


ets_forecast = forecast(ets_model, h=12)
ets_forecast
# MAPE(ets_forecast$mean, validation) *100


####################################### Exponential Smoothing (additive) ############################################
ets_model_additive = ets(data, allow.multiplicative.trend = FALSE)
summary(ets_model_additive)

ets_forecast = forecast(ets_model_additive, h=12)
ets_forecast
# MAPE(ets_forecast$mean, validation) *100


############################################ Double Seasonal holt winters ###############################

dshw_model = dshw(data, period1=2, period2 = 3, h=12)
#MAPE(dshw_model$mean, validation)*100


#############################################  ARIMA ######################################
arima_optimal = auto.arima(data)
arima_optimal
summary(arima_optimal)
arima_forecast = forecast(arima_optimal, h=12)
arima_forecast

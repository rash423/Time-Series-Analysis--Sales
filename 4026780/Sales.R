install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
sales_data <- read.csv("salesFinal.csv")
sales_ts <-ts(sales_data, start = 2020, frequency = 7)

# holt winter's
model1<- HoltWinters(sales_ts, seasonal = "additive")
plot(model1)

model2<- HoltWinters(sales_ts, seasonal = "multiplicative")
plot(model2)

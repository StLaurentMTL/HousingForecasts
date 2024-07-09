library(tidyverse)
library(lubridate)
library(car)
library(fpp3)
library(np)
library(fable.prophet)

#Season Decomposing the Median Sale Prices
Median_Sale = Quarterly_NoNA %>% select(DATE,Median_Sale_Price_UT)

Median_Sale_Price_STL = Median_Sale %>% 
  model(stl = STL(Median_Sale_Price_UT))

Median_Sale_Price_STL %>% components() %>% autoplot()

#Bootstrapping simulated series
Median_Sale_Price_STL %>% 
  generate(new_data = Median_Sale, times = 10, bootstrap_block_size = 8) %>%
  autoplot(.sim) +  
  autolayer(Median_Sale,Median_Sale_Price_UT) +
  guides(colour = "none") + 
  labs(title = "Bootstrapped Simulated Median Sale Price Series",x = "Date",y = "Median Sale Price (,000$)")

#Bootstrapped, Bagged forecasts
#Looking for ultimate bootstrap block size
optimal_length = b.star(Median_Sale$Median_Sale_Price_UT)
#4.24 optimal bootstrap block size. 

Generated_Set_2 = Median_Sale_Price_STL %>% 
  generate(new_data = Median_Sale, times = 100,
           bootstrap_block_size = 4.24) %>% 
  select(-.model,-Median_Sale_Price_UT)

#ETS forecasts
ETS_Forecast = Generated_Set_2 %>% 
  model(ets = ETS(.sim)) %>% 
  forecast(h = 8)

ETS_Set = ETS_Forecast %>% update_tsibble(key = .rep)

#ETS Plotted
ETS_Set %>% autoplot(.mean) +
  autolayer(Median_Sale,Median_Sale_Price_UT) +
  guides(colour = "none") +
  labs(title = "Bootstrapped Utah Median Home Sale Prices",
       subtitle = "ETS Forecasts",
       x = "Date",
       y = "Median Sale Price (,000 $)")

#Bagged (Averaged) Set
Bagged = ETS_Set %>% summarise(bagged_mean = mean(.mean))

Median_Sale %>% model(ETS = ETS(Median_Sale_Price_UT)) %>% 
  forecast(h = 8) %>% 
  autoplot(Median_Sale) +
  autolayer(Bagged,bagged_mean,col = "#D55E00") +
  labs(title = "Utah Median Home Sale Prices",
       subtitle = "ETS Forecasts, Bagged",
       x = "Date",
       y = "Median Sale Price (,000 $)")

#ARIMA set
ARIMA_Forecast = Generated_Set_2 %>% 
  model(ARIMA = ARIMA(.sim)) %>% 
  forecast(h = 8)

ARIMA_Set = ARIMA_Forecast %>% update_tsibble(key = .rep)

ARIMA_Set %>% autoplot(.mean) +
  autolayer(Median_Sale,Median_Sale_Price_UT) +
  guides(colour = "none") +
  labs(title = "Bootstrapped Utah Median Home Sale Price",
       subtitle = "ARIMA Forecasts",
       x = "Date",
       y = "Median Sale Price (,000 $)")

#Bagged (Averaged) Set
Bagged_ARIMA = ARIMA_Set %>% summarise(bagged_mean = mean(.mean))

Median_Sale %>% model(ARIMA = ARIMA(Median_Sale_Price_UT)) %>% 
  forecast(h = 8) %>% 
  autoplot(Median_Sale) +
  autolayer(Bagged_ARIMA,bagged_mean,col = "#D55E00") +
  labs(title = "Bagged Utah Median Home Sale Price",subtitle = "ARIMA Forecasts",
       x = "Date",y = "Median Sale Price (,000 $)")

#Facebook Prophet Model
Prophet_Forecast = Generated_Set_2 %>% 
  model(Prophet = prophet(.sim)) %>% 
  forecast(h = 8)

Prophet_Set = Prophet_Forecast %>% update_tsibble(key = .rep)

Prophet_Set %>% autoplot(.mean) +
  autolayer(Median_Sale,Median_Sale_Price_UT) +
  guides(colour = "none") +
  labs(title = "Bootstrapped Utah Median Home Sale Prices",
       subtitle = "Prophet Forecasts",
       x = "Date",
       y = "Median Sale Price (,000 $)")

#Bagged (Averaged) Set
Bagged_Prophet = Prophet_Set %>% summarise(bagged_mean = mean(.mean))

Median_Sale %>% model(Prophet = prophet(Median_Sale_Price_UT)) %>% 
  forecast(h = 8) %>% 
  autoplot(Median_Sale) +
  autolayer(Bagged_Prophet,bagged_mean,col = "#D55E00") +
  labs(title = "Bagged Utah Median Home Sale Price",subtitle = "Prophet Forecasts",
       x = "Date",y = "Median Sale Price (,000 $)")



library(tidyverse)
library(lubridate)
library(car)
library(fpp3)
library(np)
library(fable.prophet)


#Seasonal Decomposition and Set Generation
Housing_Starts = Monthly_NoNA %>% select(DATE,Housing_Starts_UT)

Housing_Starts_STL = Housing_Starts %>% 
  model(stl = STL(Housing_Starts_UT))

Housing_Starts_STL %>% components() %>% autoplot()

#Bootstrapping Simulated Series
#Selecting optimal bootstrap block size
optimal_length = b.star(Housing_Starts$Housing_Starts_UT)
#Around 15

Generated_Set = Housing_Starts_STL %>% 
  generate(new_data = Housing_Starts,times = 10,bootstrap_block_size = 14.8) %>% 
  select(-.model,-Housing_Starts_UT)

Housing_Starts_STL %>% 
  generate(new_data = Housing_Starts,times = 10,bootstrap_block_size = 14.8) %>% 
  autoplot(.sim) +
  autolayer(Housing_Starts,Housing_Starts_UT) +
  guides(colour = "none") +
  labs(title = "Simulated Utah Housing Starts",x = "Date",y = "Housing Starts")

########################ETS Forecasts
ETS_Forecast = Generated_Set %>% 
  model(ets = ETS(.sim)) %>% 
  forecast(h = 24)

ETS_Set = ETS_Forecast %>% update_tsibble(key = .rep)

#Plot
ETS_Set %>% autoplot(.mean) +
  autolayer(Housing_Starts,Housing_Starts_UT) +
  guides(colour = "none") +
  labs(title = "Bootstrapped Utah Housing Starts",
       subtitle = "ETS Forecasts",
       x = "Date",
       y = "Housing Starts")

#Bagged Set
Bagged_ETS = ETS_Set %>% summarise(bagged_mean = mean(.mean))

Housing_Starts %>% model(ETS = ETS(Housing_Starts_UT)) %>% 
  forecast(h = 24) %>% 
  autoplot(Housing_Starts) +
  autolayer(Bagged_ETS,bagged_mean,col = "#D55E00") +
  labs(title = "Bagged Utah Housing Starts",
       subtitle = "ETS Forecasts",
       x = "Date",
       y = "Housing Starts")


#################ARIMA Forecasts
ARIMA_Forecast = Generated_Set %>% 
  model(ARIMA = ARIMA(.sim)) %>% 
  forecast(h = 24)

ARIMA_Set = ARIMA_Forecast %>% update_tsibble(key = .rep)

ARIMA_Set %>% autoplot(.mean) +
  autolayer(Housing_Starts,Housing_Starts_UT) +
  guides(colour = "none") +
  labs(title = "Bootstrapped Utah Housing Starts",
       subtitle = "ARIMA Forecasts",
       x = "Date",
       y = "Housing Starts")

#Bagged Set
Bagged_ARIMA = ARIMA_Set %>% summarise(bagged_mean = mean(.mean))

Housing_Starts %>% model(ARIMA = ARIMA(Housing_Starts_UT)) %>% 
  forecast(h = 24) %>% 
  autoplot(Housing_Starts) +
  autolayer(Bagged_ARIMA,bagged_mean,col = "#D55E00") +
  labs(title = "Bagged Utah Housing Starts",
       subtitle = "ARIMA Forecasts",
       x = "Date",
       y = "Housing Starts")

###########################Facebook Prophet Model 
Prophet_Forecast = Generated_Set %>% 
  model(Prophet = prophet(.sim)) %>% 
  forecast(h = 24)

Prophet_Set = Prophet_Forecast %>% update_tsibble(key = .rep)

Prophet_Set %>% autoplot(.mean) +
  autolayer(Housing_Starts,Housing_Starts_UT) +
  guides(colour = "none") +
  labs(title = "Bootstrapped Utah Housing Starts",
       subtitle = "Prophet Forecasts",
       x = "Date",
       y = "Housing Starts")

#Bagged Set
Bagged_Prophet = Prophet_Set %>% summarise(bagged_mean = mean(.mean))

Housing_Starts %>% model(Prophet = prophet(Housing_Starts_UT)) %>% 
  forecast(h = 24) %>% 
  autoplot(Housing_Starts) +
  autolayer(Bagged_Prophet,bagged_mean,col = "#D55E00") +
  labs(title = "Bagged Utah Housing Starts",
       subtitle = "Prophet Forecasts",
       x = "Date",
       y = "Housing Starts")

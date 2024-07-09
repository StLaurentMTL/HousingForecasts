library(fpp3)
library(tidyverse)

ARIMA_forecaster = function(Dataset,Variable, Horizon, Title, Subtitle, xlab, ylab) {
  variable_sym = ensym(Variable)
  
  Set_Name = Dataset %>% 
    select(DATE, !!variable_sym) %>% 
    filter(!is.na(!!variable_sym))
  
  Model = Set_Name %>% 
    model(ARIMA(!!variable_sym))
  
  Forecast = Model %>% 
    forecast(h = Horizon)
  
  Model_Specs = Model %>% 
    report()
  
  Plot = Forecast %>%
    autoplot(Set_Name) +
    geom_line(aes(y = .fitted), col = "#D55E00", data = augment(Model)) +
    ggtitle(Title, Subtitle) +
    xlab(xlab) + ylab(ylab)
  
  list(Model = Forecast, Plot = Plot)
}


#Median Home Sale Price
Median_Sale_Price_Model_ARIMA = ARIMA_forecaster(Quarterly,
                                                 "Median_Sale_Price_UT",
                                                 8,
                                                 "Utah Median Home Sale Price",
                                                 "ARIMA Model, 2-year Forecast",
                                                 "Date",
                                                 "Median Home Sale Price, (,000$)")

Median_Sale_Price_Model_ARIMA$Plot
Median_Sale_Price_Model_ARIMA$Model

#Housing Starts
Housing_Starts_Model_ARIMA = ARIMA_forecaster(Monthly,
                                              "Housing_Starts_UT",
                                              24,
                                              "Utah Housing Starts",
                                              "ARIMA Model, 2-year Forecast",
                                              "Date",
                                              "Housing Starts")

Housing_Starts_Model_ARIMA$Plot
Median_Sale_Price_Model_ARIMA$Model

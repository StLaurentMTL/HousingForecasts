library(fpp3)
library(tidyverse)
library(fable.prophet)

Prophet_Forecaster = function(Dataset,Variable, Horizon, Title, Subtitle, xlab, ylab) {
  variable_sym = ensym(Variable)
  
  Set_Name = Dataset %>% 
    select(DATE, !!variable_sym) %>% 
    filter(!is.na(!!variable_sym))
  
  Model = Set_Name %>% 
    model(prophet(!!variable_sym))
  
  Forecast = Model %>% 
    forecast(h = Horizon)
  
  Plot = Forecast %>%
    autoplot(Set_Name) +
    geom_line(aes(y = .fitted), col = "#D55E00", data = augment(Model)) +
    ggtitle(Title, Subtitle) +
    xlab(xlab) + ylab(ylab)
  
  
  
  list(Model = Forecast,Plot = Plot)
}

#Median Home Sale Price 
Median_Sale_Price_Model_Prophet = Prophet_Forecaster(Quarterly,
                                                     "Median_Sale_Price_UT",
                                                     8,
                                                     "Utah Median Home Sale Price",
                                                     "Prophet Model, 2-year Forecast",
                                                     "Date",
                                                     "Median Home Sale Price, (,000$)")

Median_Sale_Price_Model_Prophet$Plot

#Housing Starts
Housing_Starts_Model_Prophet = Prophet_Forecaster(Monthly,
                                                  "Housing_Starts_UT",
                                                  24,
                                                  "Utah Housing Starts",
                                                  "Prophet Model, 2-year Model",
                                                  "Date",
                                                  "Housing Starts")

Housing_Starts_Model_Prophet$Plot

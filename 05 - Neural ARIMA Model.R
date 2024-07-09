library(fpp3)
library(tidyverse)

Neural_Forecaster = function(Dataset,Variable, Horizon, Title, Subtitle, xlab, ylab) {
  variable_sym = ensym(Variable)
  
  Set_Name = Dataset %>% 
    select(DATE, !!variable_sym) %>% 
    filter(!is.na(!!variable_sym))
  
  Model = Set_Name %>% 
    model(NNETAR(!!variable_sym))
  
  Model_Specs = Model %>% report()
  
  Forecast = Model %>% 
    forecast(h = Horizon)
  
  Plot = Forecast %>%
    autoplot(Set_Name) +
    geom_line(aes(y = .fitted), col = "#D55E00", data = augment(Model)) +
    ggtitle(Title, Subtitle) +
    xlab(xlab) + ylab(ylab)
  
  list(Model = Forecast,Plot = Plot,Specs = Model_Specs)
}

#Median Sale Price
Median_Sale_Price_Model_Neural = Neural_Forecaster(Quarterly,
                                                     "Median_Sale_Price_UT",
                                                     8,
                                                     "Utah Median Home Sale Price",
                                                     "Neural ARIMA Model, 2-year Forecast",
                                                     "Date",
                                                     "Median Home Sale Price, (,000$)")

Median_Sale_Price_Model_Neural$Plot

#Housing Starts
Housing_Starts__Model_Neural = Neural_Forecaster(Monthly,
                                                 "Housing_Starts_UT",
                                                 24,
                                                 "Utah Housing Starts",
                                                 "Neural ARIMA Model, 2-year Forecast",
                                                 "Date",
                                                 "Housing Starts")

Housing_Starts__Model_Neural$Plot

library(fpp3)
library(tidyverse)
library(lubridate)

#Function
ETS_forecaster = function(Dataset,Variable, Horizon, Title, Subtitle, xlab, ylab) {
  variable_sym = ensym(Variable)
  
  Set_Name = Dataset %>% 
    select(DATE, !!variable_sym) %>% 
    filter(!is.na(!!variable_sym))
  
  Model = Set_Name %>% 
    model(ETS(!!variable_sym))
  
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
Median_Sales_Price_Model_ETS = ETS_forecaster(Quarterly,
                                              "Median_Sale_Price_UT",
                                              8,
                                              "Utah Median Home Sale Price",
                                              "ETS Model, 2-year Forecast",
                                              "Date",
                                              "Median Home Sale Price, (,000$)")

Median_Sales_Price_Model_ETS$Plot
Median_Sales_Price_Model_ETS$Model

#Housing Starts
Housing_Starts_Model_ETS = ETS_forecaster(Monthly,
                                          "Housing_Starts_UT",
                                          24,
                                          "Utah Housing Starts",
                                          "ETS Model, 2-year Forecast",
                                          "Date",
                                          "Housing Starts")

Housing_Starts_Model_ETS$Plot
Housing_Starts_Model_ETS$Model


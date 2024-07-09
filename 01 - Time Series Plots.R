library(fpp3)
library(tidyverse)

#This function will be able to make time series plots
#of each of the variables contained within each dataset

#Function
Time_Series <- function(dataset,y_var,title,y_name){
  Plot <- ggplot(data = dataset, aes(x = DATE, y = !!rlang::sym(y_var))) +
    geom_line() +
    labs(title = title,
         x = "Year",
         y = y_name) #+
    #geom_smooth()
  print(Plot)
  return(Plot)
}

#Median Home Sale Price (Quarterly)
Time_Series(Quarterly,
            "Median_Sale_Price_UT",
            "Median Home Sale Price",
            "Sale Price (,000$)")

#Housing Starts (Monthly)
Time_Series(Monthly,
            "Housing_Starts_UT",
            "Housing Starts",
            "Housing Starts")

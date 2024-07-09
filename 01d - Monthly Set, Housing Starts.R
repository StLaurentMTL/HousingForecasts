library(tidyverse)
library(readxl)
library(lubridate)

import = function(path,seriesname,newname){
  Set_1 = read.csv(paste0(path))
  Set_2 = Set_1 %>% mutate(DATE = yearmonth(DATE)) %>% rename(!!newname := !!sym(seriesname))
  return(Set_2)
}

PPI = import("Input Filepath/PPI Net Inputs to Residential Construction, Goods - Monthly.csv",
       "WPUIP2311001",
       "PPI")

Revolving_Home_Loan_Monthly = import("Input Filepath/Real Estate Loans - Residential Real Estate Loans - Revolving Home Equity Loans, All Commercial Banks - Monthly.csv",
                                     "RHEACBW027SBOG",
                                     "Revolving_Home_Loan_Monthly")

SP500_Monthly = import("Input Filepath/SP500 - Monthly.csv",
                       "SP500",
                       "SP500_Monthly")

Total_Assets_Monthly = import("Input Filepath/Total Assets, All Commercial Banks - Monthly.csv",
                              "TLAACBW027SBOG",
                              "Total_Assets_Monthly")

Consumer_Conf_Monthly = import("Input Filepath/UMich Consumer Confidence - Monthly.csv",
                               "UMCSENT",
                               "Consumer_Conf_Monthly")

Unemployment_Rate_Monthly = import("Input Filepath/Unemployment Rate - Utah - Monthly.csv",
                                   "UTUR",
                                   "Unemployment_Rate_Monthly")

Moodys_Monthly = read.csv("Input Filepath/UT - Moodys - Monthly.csv")
Moodys_Monthly = Moodys_Monthly %>% mutate(DATE = yearmonth(Time)) %>% select(-Time)


Monthly_Regression_Set = Consumer_Conf_Monthly %>% 
  left_join(Moodys_Monthly,by = "DATE") %>% 
  left_join(PPI,by = "DATE") %>% 
  left_join(Revolving_Home_Loan_Monthly,by = "DATE") %>% 
  left_join(SP500_Monthly,by = "DATE") %>% 
  left_join(Total_Assets_Monthly,by = "DATE") %>% 
  left_join(Unemployment_Rate_Monthly,by = "DATE")

write.csv(Monthly_Regression_Set,"Output Filepath/Monthly Regression Set.csv")

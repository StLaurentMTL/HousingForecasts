library(tidyverse)
library(readxl)

directory = #Filepath here, consult "input" folder. 

import = function(filename,original_var,name){
  data = read.csv(paste0(directory,filename))
  data_1 = data %>% rename(!!name := !!original_var)
  return(data_1)
}

#Consumer Confidence
Consumer = import("UMich Consumer Confidence.csv",
                  "UMCSENT",
                  "Consumer_Conf")

#Treasury 10-Year Yield
Treasury_Yield = import("Treasury Yield 10 Year.csv",
                        "BOGZ1FL073161113Q",
                        "Treasury_Yield")

#Total Assets, Commercial Banks
Total_Assets = import("Total Assets, All Commercial Banks (ANNUAL).csv",
                      "TLAACBW027SBOG",
                      "Total_Assets")

#S&P 500 Index
SP5000 = import("S&P 500 Index - Averaged.csv","SP500","SP500")

#Revolving Home Equity Loans, All Commercial Banks"
Revolving_Home_Loans = import("Real Estate Loans - Residential Real Estate Loans - Revolving Home Equity Loans, All Commercial Banks (ANNUAL).csv",
                              "RHEACBW027SBOG",
                              "Revolving_Home_Loans")

#PPI Net Inputs to Residential Construction
PPI = import("PPI Net Inputs to Residential Construction, Goods.csv",
             "WPUIP2311001",
             "PPI")

#Households - Net Worth
Household_net_worth = import("Households - Net Worth, Level (ANNUAL).csv",
                             "BOGZ1FL192090005Q",
                             "Household_net_worth")

#Household Debt Service Payments as a Percent of Disposable Personal Income
Household_debt = import("Household Debt Service Payments as a Percent of Disposable Personal Income (ANNUAL).csv",
                        "TDSP",
                        "Household_debt")

FRED = Consumer %>% 
  left_join(Household_debt,by = "DATE") %>% 
  left_join(Household_net_worth,by = "DATE") %>% 
  left_join(PPI,by = "DATE") %>% 
  left_join(Revolving_Home_Loans,by = "DATE") %>% 
  left_join(SP5000,by = "DATE") %>% 
  left_join(Total_Assets,by = "DATE") %>% 
  left_join(Treasury_Yield,by = "DATE")

write.csv(FRED,"Output Filepath Here/FRED.csv")
  



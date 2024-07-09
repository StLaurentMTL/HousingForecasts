library(tidyverse)
library(readxl)
library(fpp3)

#Moodys
Moodys_Quarterly = read.csv("Input Filepath/Moody's Basket of Data - Quarterly.csv")

Moodys_Quarterly = Moodys_Quarterly %>% mutate(DATE = yearquarter(DATE))

#FRED Data
FRED_import = function(Dataset_Name,Path,Series_Name){
  Data_1 = read.csv(Path)
  
  Data_2 = Data_1 %>% mutate(DATE = yearquarter(DATE)) %>% 
    rename(!!Dataset_Name := !!sym(Series_Name))
  
  return(Data_2)
}

#Household_net_worth
Household_net_worth = FRED_import("Household_net_worth",
                                  "Input Filepath/Quarterly Set/Households - Net Worth, Level (QUARTERLY).csv",
                                  "BOGZ1FL192090005Q")

#PPI
PPI = FRED_import("PPI",
                  "Input Filepath/Quarterly Set/PPI Net Inputs to Residential Construction, Goods - Quarterly.csv",
                  "WPUIP2311001")

#Revolving_Home_Loans
Revolving_Home_Loans = FRED_import("Revolving_Home_Loans",
                                   "Input Filepath/Quarterly Set/Real Estate Loans - Residential Real Estate Loans - Revolving Home Equity Loans, All Commercial Banks - Quarterly.csv",
                                   "RHEACBW027SBOG")

#SP500
SP500 = FRED_import("SP500",
                    "Input Filepath/Quarterly Set/SP500 - Quarterly.csv",
                    "SP500")
#Total_Assets
Total_Assets = FRED_import("Total_Assets",
                           "Input Filepath/Quarterly Set/Total Assets, All Commercial Banks - Quarterly.csv",
                           "TLAACBW027SBOG")

#Treasury_Yield
Treasury_Yield = FRED_import("Treasury_Yield",
                             "Input Filepath/Quarterly Set/Treasury Yield 10 Year - Quarterly.csv",
                             "BOGZ1FL073161113Q")

#Consumer_Conf
Consumer_Conf = FRED_import("Consumer_Conf",
                            "Input Filepath/Quarterly Set/UMich Consumer Confidence - Quarterly.csv",
                            "UMCSENT")


#Median_Sale_Price_UT_avg
Moodys_Quarterly_2 = read_xlsx("Input Filepath/Quarterly Set/UT - Moodys - Quarterly.xlsx")
Moodys_Quarterly_2 = Moodys_Quarterly_2 %>% mutate(DATE = yearquarter(Time)) %>% select(-Outlook,
                                                                                        -Time,
                                                                                        -Affordability_Index_UT)


#Merge
Quarterly = Consumer_Conf %>% left_join(Treasury_Yield,by = "DATE") %>% 
  left_join(Total_Assets,by = "DATE") %>% 
  left_join(Household_net_worth,by = "DATE") %>% 
  left_join(PPI,by = "DATE") %>% 
  left_join(Revolving_Home_Loans,by = "DATE") %>% 
  left_join(SP500,by = "DATE") %>% 
  left_join(Moodys_Quarterly,by = "DATE") %>% 
  left_join(Moodys_Quarterly_2, by = "DATE")

Quarterly = Quarterly %>% select(-CER_UT,-Rental_Vacancy_UT_Avg,-Foreclosed_UT_Avg)

write.csv(Quarterly,"Output Filepath/Quarterly Set.csv")

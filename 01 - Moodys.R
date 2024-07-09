library(tidyverse)
library(readxl)

#Importing
directory = #Filepath for directory here. Consult "input" folder.

import = function(filename){
  data = read_excel(paste0(directory,filename))
  return(data)
}

#Annual
Annual = import("UT - Moodys - Annual.xlsx")
#Monthly
Monthly = import("UT - Moodys - Monthly.xlsx")
#Quarterly
Quarterly = import("UT - Moodys - Quarterly.xlsx")

#Preliminary Cleanups

#Annual
Annual$Year = year(Annual$Time)

Annual_1 = Annual %>% 
            select(Year,
                   Immigration_UT,
                   Net_Migration_UT,
                   Mean_Income_UT,
                   Median_Income_UT,
                   Housing_Stock_UT) 

Annual_1$Year = as.character(Annual_1$Year)

#Monthly
Monthly$Year = year(Monthly$Time)
Monthly$Month = month(Monthly$Time)

summary(Monthly)

Monthly_1 = Monthly %>% 
              select(Year,
                     Housing_Starts_UT,
                     Housing_Permits_UT,
                     CPI)

Monthly_Avg = Monthly_1 %>% 
              group_by(Year) %>% 
              summarise(Housing_Starts_UT_Avg = mean(Housing_Starts_UT),
                        Housing_Permits_UT_Avg = mean(Housing_Permits_UT),
                        CPI_Avg = mean(CPI))

Monthly_Avg$Year = as.character(Monthly_Avg$Year)

#Quarterly

Quarterly$Year = substring(Quarterly$Time,1,4)

summary(Quarterly)

Quarterly_Avg = Quarterly %>% 
                group_by(Year) %>% 
                summarise(CER_UT_Avg = mean(CER_UT),
                          Affordability_Index_UT_Avg = mean(Affordability_Index_UT),
                          Rental_Vacancy_UT_Avg = mean(Rental_Vacancy_UT),
                          Outlook_Avg = mean(Outlook),
                          Foreclosed_UT_Avg = mean(Foreclosed_UT),
                          Median_Sale_Price_UT_Avg = mean(Median_Sale_Price_UT))
                

#Inner Joins

Moodys = Annual_1 %>% 
  left_join(Monthly_Avg, by = "Year") %>% 
  left_join(Quarterly_Avg, by = "Year")


Moodys$DATE = as.Date(paste0(Moodys$Year,"-01-01"))

Moodys = Moodys %>% select(-Year)

#export
write.csv(Moodys,file = "#Output Filepath Here/Moodys.csv")


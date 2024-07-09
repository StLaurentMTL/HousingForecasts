library(tidyverse)

#HMI (Home Market Index)
HMI = read.csv("Input Filepath/HPI_AT_state.csv")

HMI_1 = subset(HMI, HMI$State == "UT") %>% 
  group_by(Year) %>% 
  summarise(HDI_avg = mean(HDI)) %>% 
  rename(DATE = Year)

HMI_1$DATE = as.Date(paste0(HMI_1$DATE,"-01-01"))

#Unemployment Rate, UT
Unemployment_Rate = read.csv("Input Filepath/Unemployment Rate - Utah.csv")

Unemployment_Rate_1 = Unemployment_Rate %>% select(Year,Period,Label,Value)

Unemployment_Rate_2 = Unemployment_Rate_1 %>% 
  group_by(Year) %>% 
  summarise(Unemployment_Rate_Avg = mean(Value)) %>% 
  rename(DATE = Year)

Unemployment_Rate_2$DATE = as.Date(paste0(Unemployment_Rate_2$DATE,"-01-01"))

Other = HMI_1 %>% left_join(Unemployment_Rate_2,by = "DATE")

write.csv(Other,"Output Filepath/Other.csv")

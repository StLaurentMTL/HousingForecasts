library(tidyverse)
library(fpp3)

Quarterly = read.csv("Output Filepath/Quarterly Set.csv")

Quarterly = Quarterly %>% 
  mutate(DATE = yearquarter(DATE)) %>% 
  as_tsibble(index = DATE) %>% 
  select(-X)

#Quarterly set with no NAs (for models and graphs that require complete datasets)
Quarterly_NoNA = na.omit(Quarterly)
#Quarterly set with no NAs removing S&P500 series (as it's the most limiting range of data)
Quarterly_NoNA_NoSP = Quarterly %>% select(-SP500)
Quarterly_NoNA_NoSP = na.omit(Quarterly_NoNA_NoSP)

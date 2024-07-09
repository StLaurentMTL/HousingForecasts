library(tidyverse)
library(fpp3)

Monthly = read.csv("Output Filepath/Monthly Regression Set.csv")

Monthly = Monthly %>% 
  mutate(DATE = yearmonth(DATE)) %>% 
  as_tsibble(index = DATE) %>% 
  select(-X)

#Monthly set with no NAs (for models and graphs that require complete datasets)

Monthly_NoNA = na.omit(Monthly)

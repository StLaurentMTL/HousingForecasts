library(tidyverse)
library(fpp3)

Master = read.csv("Output Filepath/Master.csv")

Master_tsibble = Master %>% 
  mutate(DATE = year(DATE)) %>% 
  as_tsibble(index = DATE) 

Master_tsibble = Master_tsibble %>% select(-X.1,-X.y,-X)


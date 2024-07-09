library(tidyverse)

#Merge
#Moodys
Moodys = read.csv("Output Filepath/Moodys.csv")

#FRED
FRED = read.csv("Output Filepath/FRED.csv")

#Other
Other = read.csv("Output Filepath/Other.csv")

#Merging
Master = FRED %>% left_join(Moodys,by = "DATE") %>% left_join(Other,by = "DATE")

Master = Master %>% select(-X.x,-X.y,-X)

Master = Master %>% mutate(Starts_to_Stock_Ratio = Housing_Starts_UT_Avg/Housing_Stock_UT)

#Converting to "." to NAs
Master$Consumer_Conf = as.numeric(Master$Consumer_Conf)
Master$Revolving_Home_Loans = as.numeric(Master$Revolving_Home_Loans)
Master$Total_Assets = as.numeric(Master$Total_Assets)
Master$Median_Income_UT = as.numeric(Master$Median_Income_UT)
Master$PPI = as.numeric(Master$PPI)

#Exporting Master
write.csv(Master,"Output Filepath/Master.csv")


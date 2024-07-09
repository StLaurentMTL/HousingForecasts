library(tidyverse)
library(car)
library(tsibble)
library(fpp3)

######################################################Housing Starts
#Data preparation (for forecasting and visualizations)
Monthly_Regression = new_data(Monthly_NoNA,8) %>% 
  mutate(Consumer_Conf_Monthly = mean(Monthly_NoNA$Consumer_Conf_Monthly),
         Unemployment_Rate_Monthly = mean(Monthly_NoNA$Unemployment_Rate_Monthly),
         Housing_Permits_UT = mean(Monthly_NoNA$Housing_Permits_UT),
         PPI = mean(Monthly_NoNA$PPI),
         SP500_Monthly = mean(Monthly_NoNA$SP500_Monthly),
         Revolving_Home_Loan_Monthly = mean(Monthly_NoNA$Revolving_Home_Loan_Monthly),
         CPI = mean(Monthly_NoNA$CPI),
         Total_Assets_Monthly = mean(Monthly_NoNA$Total_Assets_Monthly))

#Model Selection, beginning with all explanatory variables and using
#bi-directional stepwise model selection. We are using AICcc to determine the
#most appropriate selection. 
All_Vars_Housing_Starts = lm(Housing_Starts_UT ~.-DATE-Housing_Starts_UT,data = Monthly_NoNA)
summary(All_Vars_Housing_Starts)

Selection_Housing_Starts = step(All_Vars_Housing_Starts,direction = "both")

#Best fit designation is: Housing_Starts_UT ~ CPI + Revolving_Home_Loan_Monthly + SP500_Monthly + 
#Unemployment_Rate_Monthly

Best_Fit_Housing_Starts = Monthly_NoNA %>% 
  model("Best" = TSLM(Housing_Starts_UT ~ CPI + Revolving_Home_Loan_Monthly + SP500_Monthly + 
                                                            Unemployment_Rate_Monthly))

Best_Fit_Housing_Starts %>% report() #The direction of the variations seem strange however
Best_Fit_Housing_Starts %>% gg_tsresiduals() #There is autocorrelation for the first lag.

#Plotting the forecasting based on hypothetical future values
augment(Best_Fit_Housing_Starts) %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = Housing_Starts_UT,colour = "Data")) +
  geom_line(aes(y = .fitted,colour = "Fitted")) +
  ggtitle("Utah Housing Starts", "Time Series OLS model fitting") +
  xlab("Date") + ylab("Housing Starts") +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL)) 
  
Best_Fit_Housing_Starts %>% 
  forecast(new_data = Monthly_Regression) %>% 
  autoplot(Monthly_NoNA,level = 95) + labs(title = "Utah Housing Starts",subtitle = "OLS model, 2-year Forecast",
       x = "Date",y = "Housing Starts") #Not a great fit

#Model Regression based on trend (the series does not lend well to seasonality)

Trend_Housing_Starts = Monthly_NoNA %>% 
  model("Trend and Season" = TSLM(Housing_Starts_UT ~ trend()))

Trend_Housing_Starts %>% report() #Holding all else constant, each month
#brings an average of 117.40 more Housing Starts
Trend_Housing_Starts %>% gg_tsresiduals() #Lot of autocorrelation however

Trend_Housing_Starts %>% 
  forecast(new_data = Monthly_Regression) %>% 
  autoplot(Monthly_NoNA,level = 95)

#ARIMA model using best fit variables
ARIMA_Housing_Starts = Monthly_NoNA %>% 
  model("ARIMA" = ARIMA(Housing_Starts_UT ~ CPI + Revolving_Home_Loan_Monthly + SP500_Monthly + 
                                                              Unemployment_Rate_Monthly))
Results_ARIMA = ARIMA_Housing_Starts %>% report()
ARIMA_Housing_Starts %>% gg_tsresiduals() #Much less autocorrelation

#Plotting the forecasting based on hypothetical future values
augment(ARIMA_Housing_Starts) %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = Housing_Starts_UT,colour = "Data")) +
  geom_line(aes(y = .fitted,colour = "Fitted")) +
  ggtitle("Utah Housing Starts", "ARIMA regression model fitting") +
  xlab("Date") + ylab("Housing Starts") +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

ARIMA_Housing_Starts %>% 
  forecast(new_data = Monthly_Regression) %>% 
  autoplot(Monthly_NoNA,level = 95) +
  labs(title = "Utah Housing Starts",
       subtitle = "ARIMA Regression model, 2-year Forecast",
       x = "Date",
       y = "Housing Starts")


########################################################Median Sale Price
Quarterly_Regression = Forecast_Set = new_data(Quarterly_NoNA,8) %>% 
  mutate(Consumer_Conf = mean(Quarterly_NoNA$Consumer_Conf),
         Treasury_Yield = mean(Quarterly_NoNA$Treasury_Yield),
         Total_Assets = mean(Quarterly_NoNA$Total_Assets),
         Household_net_worth = mean(Quarterly_NoNA$Household_net_worth),
         PPI = mean(Quarterly_NoNA$PPI),
         Revolving_Home_Loans = mean(Quarterly_NoNA$Revolving_Home_Loans),
         CER_UT_Avg = mean(Quarterly_NoNA$CER_UT_Avg),
         Affordability_Index_UT_Avg = mean(Quarterly_NoNA$Affordability_Index_UT_Avg),
         HMI = mean(Quarterly_NoNA$HMI),
         Net_Migration_UT = mean(Quarterly_NoNA$Net_Migration_UT),
         Rental_Vacancy_UT = mean(Quarterly_NoNA$Rental_Vacancy_UT),
         Foreclosed_UT = mean(Quarterly_NoNA$Foreclosed_UT),
         Median_Sale_Price_UT = mean(Quarterly_NoNA$Median_Sale_Price_UT))

#Model Selection, beginning with all explanatory variables and using
#bi-directional stepwise model selection. We are using AICcc to determine the
#most appropriate selection. 
All_Vars_Median_Sale_Price = lm(Median_Sale_Price_UT ~.-DATE-Median_Sale_Price_UT,data = Quarterly_NoNA_NoSP)
summary(All_Vars_Median_Sale_Price)

Selection_Housing_Starts = step(All_Vars_Median_Sale_Price,direction = "both")

#Best fit designation is Median_Sale_Price_UT ~ Consumer_Conf + Treasury_Yield + Total_Assets + 
#Household_net_worth + PPI + Revolving_Home_Loans + CER_UT_Avg + 
  #Affordability_Index_UT_Avg + HMI

Best_Fit_Median_Sale_Price = Quarterly %>% model("Best" = TSLM(Median_Sale_Price_UT ~ Consumer_Conf + Treasury_Yield + Total_Assets + 
                                                                 Household_net_worth + PPI + Revolving_Home_Loans + CER_UT_Avg + 
                                                                 Affordability_Index_UT_Avg + HMI))

Best_Fit_Median_Sale_Price %>% report() #adjusted R^2 is absurdly high
Best_Fit_Median_Sale_Price %>% gg_tsresiduals() #It makes sense
#that is some pretty bad autocorrelation going on. 

augment(Best_Fit_Median_Sale_Price) %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = Median_Sale_Price_UT,colour = "Data")) +
  geom_line(aes(y = .fitted,colour = "Fitted")) +
  ggtitle("Utah Median Home Sale Price", "Time Series OLS model fitting") +
  xlab("Date") + ylab("Median Home Sale Price (,000$)") +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL)) #Practically overlapping

Best_Fit_Median_Sale_Price %>% 
  forecast(new_data = Quarterly_Regression) %>% 
  autoplot(Quarterly_NoNA_NoSP,level = 95) + labs(title = "Utah Median Home Sale Price",subtitle = "OLS model, 2-year Forecast",
                                           x = "Date",y = "Median Home Sale Price (,000$)")

#Bad prediction 

#ARIMA model using best fit variables
ARIMA_Median_Sale_Price = Quarterly %>% model(ARIMA = ARIMA(Median_Sale_Price_UT ~ Consumer_Conf + Treasury_Yield + Total_Assets + 
                                                              Household_net_worth + PPI + Revolving_Home_Loans + CER_UT_Avg + 
                                                              Affordability_Index_UT_Avg + HMI))

ARIMA_Median_Sale_Price %>% report()
ARIMA_Median_Sale_Price %>% gg_tsresiduals() #autocorrelation isn't much better either 

#Plot
augment(ARIMA_Median_Sale_Price) %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = Median_Sale_Price_UT,colour = "Data")) +
  geom_line(aes(y = .fitted,colour = "Fitted")) +
  ggtitle("Utah Median Home Sale Price", "ARIMA regression model fitting") +
  xlab("Date") + ylab("Median Home Sale Price (,000$)") +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL)) #Overlapping, this is strange



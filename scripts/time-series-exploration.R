library(lubridate) # Handling dates
library(stringr) # Handling strings
library(readr) # Reading csv files
library(tidyr) # Data manipulation
library(dplyr) # Data manipulation
library(ggplot2) # Plots
library(forcats) # Factors
library(knitr) # Clean tables
library(stats)
library(ggfortify)
library(scales)
library(forecast)

## Note setup:
# Add your data folder into this directory and
read.prep.data <- function() {
  rossman.train <- read_csv(file = './data/train.csv',
                            col_types = 'icDiiiici')
  rossman.test <- read_csv(file = './data/test.csv',
                           col_types = 'iicDcccc')
  rossman.store <- read_csv(file = './data/store.csv',
                            col_types = 'icciiiciic')
  
  # Reformat and clean up data
  
  rossman.store <-
    rossman.store %>%
    mutate(Assortment = fct_recode(Assortment,
                                   "Basic" = "a",
                                   "Extra" = "b",
                                   "Extended" = "c"))
  
  rossman.train <-
    rossman.train %>%
    mutate(DayOfWeek = fct_recode(DayOfWeek,
                                  "Monday" = "1" ,
                                  "Tuesday" = "2" ,
                                  "Wednesday" = "3" ,
                                  "Thursday" = "4" ,
                                  "Friday" = "5" ,
                                  "Saturday" = "6" ,
                                  "Sunday" = "7" ),
           StateHoliday = fct_recode(StateHoliday,
                                     "None" = "0",
                                     "Public" = "a",
                                     "Easter" = "b",
                                     "Christmas" = "c"))
  return(list(rossman.train, rossman.test, rossman.store))
}

rossman.data <- read.prep.data()
rossman.train <- rossman.data[[1]]
rossman.test <- rossman.data[[2]]
rossman.store <- rossman.data[[3]]
rm(rossman.data)

store.1 <-
  rossman.train %>% 
  filter(Store == 1) 

# Exploration

store.1 %>%
  select(Date, Sales, DayOfWeek) %>%
  ggplot() +
  geom_line(mapping = aes(x = Date,
                          y = Sales),
            color = "#3498db") +
  geom_hline(mapping = aes(yintercept = mean(Sales)),
             color = '#e74c3c',
             show.legend = T) + 
  ggtitle(label = "Store 20: Daily Sales")

store.1 %>%
  select(Date, Sales, DayOfWeek) %>%
  filter(Date < "2013-02-01") %>%
  ggplot() +
  geom_line(mapping = aes(x = Date,
                          y = Sales),
            color = "#3498db") +
  geom_hline(mapping = aes(yintercept = mean(Sales)),
             color = '#e74c3c',
             show.legend = T) + 
  ggtitle(label = "Store 20: Daily Sales in January, 2013")

store.1 %>%
  group_by(DayOfWeek) %>%
  summarize(Avg_Sales = mean(Sales)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = DayOfWeek,
                         y = Avg_Sales),
           fill = "#3498db",
           stat = "identity") +
  ggtitle(label = "Store 20: Daily Sales in January, 2013")

# Fitting the forecast model

forecast_fit <- function(d) {
  Sales <- ts(d$Sales, frequency = 10)
  lambda <- BoxCox.lambda(Sales)
  tsclean(Sales, replace.missing = TRUE, lambda = lambda)
  # External regressors to be used in the ARIMA model
  # xreg <- 
  #   d %>%
  #   mutate(Open = as.numeric(Open),
  #          Promo = as.numeric(Open)) %>%
  #   select(c(Open, Promo))
  fit <- forecast(Sales, lambda = lambda)
  return(fit)
}

f <- forecast_fit(store.1)
View(round(f$mean, 2))
summary(store.1)





## Seasonality = 7



# Time series exploration and forecasting

## Load libraries
{
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
  library(tibble)
}

## Read and prepare data
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

{
  rossman.data <- read.prep.data()
  rossman.train <- rossman.data[[1]]
  rossman.test <- rossman.data[[2]]
  rossman.store <- rossman.data[[3]]
  rm(rossman.data)
}

## Get data for a store, store 1
{
  store.1 <-
  rossman.train %>% 
  filter(Store == 1) 
}

## Exploration
{
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
}

## Generic function for fitting model
forecast_fit <- function(d, take.log = T, func = ets) {
  d <-
    d %>%
    mutate(Sales = ifelse(Sales > 0,
                          log(Sales),
                          0))
  
  Sales <- ts(d$Sales[1:(nrow(d) - 42)], frequency = 7)
  lambda <- BoxCox.lambda(Sales)
  tsclean(Sales, replace.missing = TRUE, lambda = lambda)
  # External regressors to be used in the ARIMA model
  # xreg <-
  #   d %>%
  #   head((nrow(d) - 42)) %>%
  #   mutate(Open = as.numeric(Open),
  #          Promo = as.numeric(Open)) %>%
  #   select(c(Open, Promo))
  fit <- func(Sales, lambda = lambda)
  return(fit)
}

## Arima and ets models
{  
  fit_arima <- forecast_fit(store.1, auto.arima)
  fit_ets <- forecast_fit(store.1)
  
  forecast_arima <- forecast(object = fit_arima, h = 42)
  forecast_ets <- forecast(object = fit_ets, h = 42)
  
  # Function to quick check predictions
  get_mse <- function(predicted, actual){
    diff <- predicted - actual
    a <- cbind(predicted = predicted,
               actual = actual,
               diff = diff)
    a <- as_tibble(a)
    View(a)
    return(mean(diff^2))
  }
  
  pred.arima <- round(forecast_arima$mean, 2)
  pred.ets <- round(forecast_ets$mean, 2)
  actual <- tail(store.1$Sales, 42)
  actual <- ifelse(actual > 0, log(actual), 0)
  
  get_mse(pred.arima, actual)
  get_mse(pred.ets, actual)
}

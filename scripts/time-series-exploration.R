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
  library(purrr)
  library(cluster)
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


## Cluster data
bins = 20
{
  store.clusters <- 
    rossman.train %>%
    group_by(Store) %>%
    summarize(Sales = mean(Sales),
              Customers = mean(Customers)) %>%
    left_join(rossman.store, by = "Store") %>%
    select(Sales, Customers, Store) %>% 
    mutate(bin = ntile(Sales, bins))
  
  store.clusters %>%
    group_by(bin) %>%
    summarize(ymax = max(Sales),
              ymin = min(Sales)) %>%
    ggplot() +
    geom_errorbar(mapping = aes(x = 30,
                                ymax = ymax,
                                ymin = ymin,
                                color = as.factor(bin)),
                  size = 3) +
    
    geom_jitter(data = store.clusters,
                mapping = aes(x = 300,
                              y = Sales,
                              color = as.factor(bin)),
                width = 250) +
    coord_flip()
}

# Prepare data for forecast method
{
  forecast_data <-
    rossman.train %>%
    left_join(store.clusters, by = "Store") %>%
    group_by(Date, bin) %>%
    summarize(Sales = mean(Sales.x))
}

## Generic function for fitting model
forecast_fit <- function(d, func = ets) {
  Sales <- ts(d$Sales, frequency = 7)
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

forecast.store <- function(bin.id) {
    store <-
      forecast_data %>% 
      filter(bin == bin.id)
    fit_ets <- forecast_fit(store)
    
    forecast_ets <- forecast(object = fit_ets, h = 48, robust = T)
    pred.ets <- forecast_ets$mean
    return(pred.ets)
}

# Submissions
{
  a <- c(1:bins) %>%
  map(forecast.store) %>%
  unlist
    
  predictions <- as_tibble(cbind(predicted = a,
                                 Date = c(rep(c(1:48), bins)),
                                 bin = sort(c(rep(c(1:bins), 48)))))
  predictions <- predictions %>%
    mutate(predicted = ifelse(predicted < 300, 0, predicted))
  
  r <- rossman.test %>%
    left_join(store.clusters, by = "Store") %>%
    select(Store, Date, bin, Id) %>%
    mutate(Date = as.numeric(Date - min(Date) + 1)) %>%
    left_join(predictions, by = c("Date" = "Date", "bin" = "bin")) %>%
    select(Id, Sales = predicted) %>%
    write_csv(path = paste0("./data/submission-", bins, ".csv"))
}
  
###################Log_linear model data prep########################################
bins1 = 20
{
  store.clusters2 <- 
    rossman.train %>%
    group_by(Store) %>%
    #summarize(Sales = mean(Sales),
     #         Customers = mean(Customers)) %>%
    left_join(rossman.store, by = "Store") %>%
    select(-Promo2SinceWeek, -Promo2, -Promo2SinceYear, -PromoInterval) %>% 
    mutate(bin = ntile(Sales, bins1))
  
  store.clusters2 %>%
    group_by(bin) %>%
    summarize(ymax = max(Sales),
              ymin = min(Sales)) %>%
    ggplot() +
    geom_errorbar(mapping = aes(x = 30,
                                ymax = ymax,
                                ymin = ymin,
                                color = as.factor(bin)),
                  size = 3) +
    
    geom_jitter(data = store.clusters2,
                mapping = aes(x = 300,
                              y = Sales,
                              color = as.factor(bin)),
                width = 250) +
    coord_flip()
}

View(store.clusters2)

library(plyr)
nonzero <- function(x) sum(x == 0)
numcolwise(nonzero)(store.cluster3)

store.cluster3 <- subset(store.clusters2, Sales != 0)



str(store.clusters2)
store.clusters2$StoreType = as.factor(store.clusters2$StoreType)

store.cluster3$log_sales = log(store.cluster3$Sales)
View(store.cluster3)

store.clusters2$bin = as.factor(store.clusters2$bin)

store.cluster3 <- store.cluster3[,-4]

#NA with means

meanCompetitionDistance <- mean(store.cluster3$CompetitionDistance, na.rm = TRUE)
store.cluster3[is.na(store.cluster3$CompetitionDistance), c("CompetitionDistance")] <- meanCompetitionDistance


model.train <- lm(log_sales ~ Date + Store + Assortment + 
                    StoreType + CompetitionDistance, data = store.cluster3, 
                  na.action = na.omit)
summary(model.train)
plot(model.train)

preds <- predict(model.train)

sqrt(mean(((exp(preds) - exp(store.cluster3$log_sales))/exp(store.cluster3$log_sales))^2))

#Preparing testing data

{
  store.clusters4 <- 
    rossman.test %>%
    group_by(Store) %>%
    #summarize(Sales = mean(Sales),
    #         Customers = mean(Customers)) %>%
    left_join(rossman.store, by = "Store") %>%
    select(-Promo2SinceWeek, -Promo2, -Promo2SinceYear, -PromoInterval)
    #mutate(bin = ntile(Store, bins3))
}
    
View(store.clusters4)

meanCompetitionDistance <- mean(store.clusters4$CompetitionDistance, na.rm = TRUE)
store.clusters4[is.na(store.clusters4$CompetitionDistance), c("CompetitionDistance")] <- meanCompetitionDistance

model.test <- predict(model.train, store.clusters4, type = "response")

####################Log Linear end##################################################
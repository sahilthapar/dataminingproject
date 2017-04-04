library(lubridate) # Handling dates
library(stringr) # Handling strings
library(readr) # Reading csv files
library(tidyr) # Data manipulation
library(dplyr) # Data manipulation
library(ggplot2) # Plots
library(forcats) # Factors

## Note setup:
# Add your data folder into this directory and

# Read files

rossman.train <- read_csv(file = './data/train.csv')
rossman.test <- read_csv(file = './data/test.csv')
rossman.store <- read_csv(file = './data/store.csv')

#Change format of variables where required
rossman.store <- rossman.store %>% mutate(StoreType = as.factor(StoreType),
                         Assortment = as.factor(Assortment),
                         Promo2 = as.factor(Promo2),
                         PromoInterval = as.factor(PromoInterval))

#Univariate analysis of CompetitionDistance variable in store.
summary(rossman.store$CompetitionDistance)
ggplot(rossman.store) + geom_histogram(aes(x=CompetitionDistance), binwidth = 10000)
ggplot(rossman.store) + geom_boxplot(aes(y=CompetitionDistance, x=""))

table(rossman.store$Assortment)
table(rossman.store$StoreType)

table(rossman.store$Promo2)
table(rossman.store$PromoInterval)

#Create a new variable CompetitionStart in date format combining
#CompetitionOpensSinceMonth and CompetitionOpensSinceYear

day <- 15
rossman.store$CompetitionStart <- str_c(rossman.store$CompetitionOpenSinceYear,"-",rossman.store$CompetitionOpenSinceMonth,"-",day)
rossman.store$CompetitionStart <- parse_date_time(rossman.store$CompetitionStart, "Y-m-d", tz = "America/New_York")

#Create a new variable PromoSinceDate in date format combining
#Promo2SinceYear and Promo2SinceWeek
rossman.store$PromoSinceDate <- as.Date(paste(rossman.store$Promo2SinceYear,rossman.store$Promo2SinceWeek, 1, sep="-"), "%Y-%U-%u")

#Remove unusable variables holding values of month and year
rossman.store[,c(5,6,8,9)] <- NULL

# Merge store data into the sales data

merged.rossman.train <-
  rossman.train %>%
  left_join(rossman.store, by = 'Store')

# Summary of the merged data

summary(merged.rossman.train)

# Transform the variables into the right format

merged.rossman.train$DayOfWeek <-
  as_factor(as.character(
    merged.rossman.train$DayOfWeek))

merged.rossman.train <-
  merged.rossman.train %>%
  mutate(DayOfWeek = fct_recode(DayOfWeek,
                                "Monday" = "1" ,
                                "Tuesday" = "2" ,
                                "Wednesday" = "3" ,
                                "Thursday" = "4" ,
                                "Friday" = "5" ,
                                "Saturday" = "6" ,
                                "Sunday" = "7" ))

merged.rossman.train %>%
  group_by(StoreType) %>%
  summarize(avgSalePerCustomer = round(mean(Sales/Customers, na.rm = T), 2),
            avgSales = mean(Sales),
            avgCustomers = mean(Customers))

# Plot the average sales vs day of week

merged.rossman.train %>%
  group_by(DayOfWeek, StoreType) %>%
  summarize(AvgSales = mean(Sales)) %>%
  ggplot() +
    geom_histogram(mapping = aes(x = DayOfWeek,
                                 y = AvgSales,
                                 fill = StoreType),
                   stat = "identity")


#Analysis of Open, State Holiday, School holiday and Store type
#changing categorical variables to factors
#For StateHoliday the values are either 0 or NA's, imputing NAs with 1 (holidays)

merged.rossman.train$StateHoliday[is.na(merged.rossman.train$StateHoliday)] <- 1

merged.rossman.train <- merged.rossman.train %>% mutate(Open = as.factor(Open),
                                          StateHoliday = as.factor(StateHoliday),
                                          SchoolHoliday = as.factor(SchoolHoliday))

table(merged.rossman.train$Open)
table(merged.rossman.train$StateHoliday)
table(merged.rossman.train$SchoolHoliday)

# Complete

# Drop in number of stores during a 6 month period

merged.rossman.train %>% 
  group_by(Date) %>% 
  summarize(n = n()) %>% 
  ggplot() +
    geom_line(mapping = aes(x = Date,
                            y = n)) +
    labs(x = "Date",
         y = "Number of stores")


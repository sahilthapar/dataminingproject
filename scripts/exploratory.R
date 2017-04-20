library(lubridate) # Handling dates
library(stringr) # Handling strings
library(readr) # Reading csv files
library(tidyr) # Data manipulation
library(dplyr) # Data manipulation
library(ggplot2) # Plots
library(forcats) # Factors
library(knitr) # Clean tables

## Note setup:
# Add your data folder into this directory and
rossman.train <- read_csv(file = './data/train.csv',
                          col_types = 'icDiicccc')
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
         Open = fct_recode(Open,
                           "Yes" = "1",
                           "No" = "0"),
         Promo = fct_recode(Promo,
                            "Yes" = "1",
                            "No" = "0"),
         StateHoliday = fct_recode(StateHoliday,
                                   "None" = "0",
                                   "Public" = "a",
                                   "Easter" = "b",
                                   "Christmas" = "c"),
         SchoolHoliday = fct_recode(SchoolHoliday,
                                    "Yes" = "1",
                                    "No" = "0"))


# Create new variables

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
 

# ------------------------------------------------------------------------------------------


# Merge data

# Merge store data into the sales data

merged.rossman.train <-
  rossman.train %>%
  left_join(rossman.store, by = 'Store')

# Drop in number of stores during a 6 month period

merged.rossman.train %>% 
  group_by(Date) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_line(mapping = aes(x = Date,
                          y = n),
            color = "#3498db") +
  labs(x = "Date",
       y = "Number of stores",
       title = "Number of stores open across time")

merged.rossman.train %>%
  filter(Store == 20) %>%
  ggplot(mapping = aes(x = Date,
                       y = Sales)) +
  geom_line(color = "#2ecc71") +
  geom_smooth() + 
  labs(title = "Store 20 Revenue across time")

kable(as.data.frame(table(rossman.store$StoreType)), col.names = c("Store Type", "Frequency"))

merged.rossman.train %>%
  group_by(StoreType) %>%
  summarize(avgSalePerCustomer = round(mean(Sales/Customers, na.rm = T), 2),
            avgSales = round(mean(Sales), 2),
            avgCustomers = round(mean(Customers), 2)) %>%
  gather(key = "Metric", value = "value", avgSales, avgSalePerCustomer, avgCustomers) %>%
  ggplot() +
  geom_bar(mapping = aes(x = StoreType,
                         y = value,
                         fill = StoreType),
           stat = "identity",
           position = "dodge",
           width = 0.5) +
  labs(x = "Store Type",
       y = "Value",
       title = "Exploring sales by Store Type") +
  facet_wrap(~ Metric, nrow = 1, scales = "free") +
  coord_flip()


kable(as.data.frame(table(rossman.store$Assortment)),
      col.names = c("Assortment", "Frequency"))

merged.rossman.train %>%
  group_by(Assortment) %>%
  summarize(avgSalePerCustomer = round(mean(Sales/Customers, na.rm = T), 2),
            avgSales = round(mean(Sales), 2),
            avgCustomers = round(mean(Customers), 2)) %>%
  gather(key = "Metric", value = "value", avgSales, avgSalePerCustomer, avgCustomers) %>%
  ggplot() +
  geom_bar(mapping = aes(x = Assortment,
                         y = value,
                         fill = Assortment),
           stat = "identity",
           position = "dodge",
           width = 0.4) +
  labs(x = "Assortment",
       y = "Value",
       title = "Exploring sales by Assortment") +
  facet_wrap(~ Metric, nrow = 1, scales = "free") +
  coord_flip()

ggplot(rossman.store) + 
  geom_histogram(aes(x=log(CompetitionDistance)),
                 binwidth = 1,
                 fill = "#2980b9")  +
  labs(x = "log(Competition Distance)",
       y = "Count",
       title = "Distribution of Log of Competition Distance")

ggplot(rossman.store) +
  geom_boxplot(aes(y=log(CompetitionDistance),
                   x=""),
               fill = "#2980b9")  +
  labs(y = "log(Competition Distance)",
       x = "",
       title = "Distribution of Log of Competition Distance (Boxplot)")

ggplot(rossman.store) +
  geom_boxplot(aes(y = log(CompetitionDistance),
                   x = StoreType,
                   fill = StoreType))   +
  labs(y = "log(Competition Distance)",
       x = "Store Type",
       title = "Distribution of Log of Competition Distance (Boxplot)")


rossman.store %>% 
  select(Promo2) %>% 
  group_by(Promo2) %>% 
  summarize(total = n()) %>% 
  ggplot() + 
  geom_bar(aes(x=Promo2,
               y=total,
               fill = Promo2),
           stat = "identity",
           width = 0.5) +
  labs(y = "Number of stores",
       x = "Promotion",
       title = "Promotion running stores")

kable(as.data.frame(table(rossman.store$Promo2, rossman.store$PromoInterval)) %>%
        filter(Var1 == 1) %>%
        select(c(Var2, Freq)),
      col.names = c("Promotion Interval", "Frequency"))

# Imputing values and converting to factors

merged.rossman.train <- merged.rossman.train %>% mutate(Open = as.factor(Open),
                                                        StateHoliday = as.factor(StateHoliday),
                                                        SchoolHoliday = as.factor(SchoolHoliday))

kable(as.data.frame(table(merged.rossman.train$Open)),
      col.names = c("Store Open", "Frequency"))





#Univariate analysis of CompetitionDistance variable in store.
summary(rossman.store$CompetitionDistance)
ggplot(rossman.store) + geom_histogram(aes(x=CompetitionDistance), binwidth = 10000)
ggplot(rossman.store) + geom_boxplot(aes(y=CompetitionDistance, x=""))

table(rossman.store$Assortment)
table(rossman.store$StoreType)

table(rossman.store$Promo2)
table(rossman.store$PromoInterval)

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

table(merged.rossman.train$StoreType, merged.rossman.train$Open, dnn = c("Store Type",
                                                                         "Store Status"))
kable(as.data.frame(table(merged.rossman.train$StateHoliday)),
      col.names = c("School Holiday", "Frequency"))

kable(as.data.frame(table(merged.rossman.train$SchoolHoliday)),
      col.names = c("School Holiday", "Frequency"))

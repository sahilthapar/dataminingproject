library(readr)
library(ggplot2)
library(lubridate)
library(stringr)
## Note setup:
#Add your data folder into this directory and
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
rossman.store$CompetetionStart <- str_c(rossman.store$CompetitionOpenSinceYear,"-",rossman.store$CompetitionOpenSinceMonth,"-",day)
rossman.store$CompetetionStart <- parse_date_time(rossman.store$Date, "Y-m-d", tz = "America/New_York")

#Create a new variable PromoSinceDate in date format combining 
#Promo2SinceYear and Promo2SinceWeek
rossman.store$PromoSinceDate <- as.Date(paste(rossman.store$Promo2SinceYear,rossman.store$Promo2SinceWeek, 1, sep="-"), "%Y-%U-%u")

#Remove unusable variables holding values of month and year
rossman.store[,c(5,6,8,9)] <- NULL


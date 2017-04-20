library(readr)

train.raw <- read_csv('./data/train.csv', col_types="ncDnncccc")
test.raw <- read_csv('./data/test.csv', col_types="nncDcccc")
store.raw <- read_csv('./data/store.csv', col_types='nccnnncnnc', na='')

# Parse features to factors
train.raw$DayOfWeek <- as.factor(train.raw$DayOfWeek)
train.raw$Open <- as.factor(train.raw$Open)
train.raw$Promo <- as.factor(train.raw$Promo)
train.raw$StateHoliday <- as.factor(train.raw$StateHoliday)
train.raw$SchoolHoliday <- as.factor(train.raw$SchoolHoliday)

store.raw$StoreType <- as.factor(store.raw$StoreType)
store.raw$Assortment <- as.factor(store.raw$Assortment)
store.raw$Promo2 <- as.factor(store.raw$Promo2)

require(dplyr)
require(lubridate)

# Join datasets by `Store` column
train.full <- left_join(train.raw, store.raw, by = 'Store')

# Filtering
train.full <- train.full %>%
  filter(Sales > 0) %>%
  filter(Open == 1) %>%
  mutate(Day = lubridate::day(Date)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  mutate(Year = lubridate::year(Date)) %>%
  mutate(LogSales = log(Sales))

# Drop unnecessary columns
train.full$Date <- NULL
train.full$Promo2SinceWeek <- NULL
train.full$PromoInterval <- NULL
train.full$Sales <- NULL
train.full$Customers <- NULL

# Remove columns below due to many NAs
train.full$CompetitionOpenSinceYear <- NULL
train.full$CompetitionOpenSinceMonth <- NULL
train.full$Promo2SinceYear <- NULL

# Fill CompetitionDistance with the average value
meanCompetitionDistance <- mean(train.full$CompetitionDistance, na.rm = TRUE)
train.full[is.na(train.full$CompetitionDistance), c("CompetitionDistance")] <- meanCompetitionDistance

require(Matrix)

train.full.sparse <- sparse.model.matrix(LogSales~.-1, data=train.full)

require(xgboost)

dtrain <- xgb.DMatrix(
  data=train.full.sparse, 
  label=train.full$LogSales)

rmpse <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab <- exp(as.numeric(labels))
  epreds <- exp(as.numeric(preds))
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}

param <- list(
  objective="reg:linear",
  booster="gbtree",
  eta=0.8, # Control the learning rate
  max.depth=8, # Maximum depth of the tree
  subsample=0.7, # subsample ratio of the training instance
  colsample_bytree=0.7 # subsample ratio of columns when constructing each tree
)

history <- xgb.cv(
  data=dtrain,
  params = param,
  early_stopping_rounds=30, # training with a validation set will stop if the performance keeps getting worse consecutively for k rounds
  nthread=4, # number of CPU threads
  nround=30, # number of trees
  verbose=0, # do not show partial info
  nfold=3, # number of CV folds
  feval=rmpse, # custom evaluation metric
  maximize=FALSE # the lower the evaluation score the better
)

x<- history$evaluation_log$iter
y1<- history$evaluation_log$train_RMPSE_mean
y2<- history$evaluation_log$test_RMPSE_mean


xgb.plot.tree(feature_names = NULL, model = history, n_first_tree = NULL,
              plot_width = NULL, plot_height = NULL)

ggplot() + geom_line(aes(x=x,y=y1), color = "blue", size=1) + 
  labs(x="No. of trees", y="RMPSE", title = "Training RMPSE with 3 fold CV")

ggplot() + geom_line(aes(x=x,y=y2), color = "red", size=1)+
  labs(x="No. of trees", y="RMPSE", title = "Test RMPSE with 3 fold CV")

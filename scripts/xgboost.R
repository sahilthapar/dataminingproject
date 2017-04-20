library(Matrix)
library(xgboost)
library(lubridate)

temp <-merged.rossman.train

temp$CompetitionStart <- as.Date(temp$CompetitionStart)

temp$CompetitionDuration <- temp$Date- temp$CompetitionStart
temp$CompetitionDuration <- as.numeric(temp$CompetitionDuration)

temp$PromoDuration <- temp$Date- temp$PromoSinceDate
temp$PromoDuration <- as.numeric(temp$PromoDuration)

temp$CompetitionStart <- NULL
temp$PromoInterval <- NULL
temp$PromoSinceDate <- NULL

temp$Store <- as.factor(temp$Store)
temp$Promo2 <- as.factor(temp$Promo2)

temp$PromoDuration[is.na(temp$PromoDuration)] <- 0
temp$CompetitionDuration[is.na(temp$CompetitionDuration)] <- 0

meanCompetitionDistance <- mean(temp$CompetitionDistance, na.rm = TRUE)
temp[is.na(temp$CompetitionDistance), c("CompetitionDistance")] <- meanCompetitionDistance

temp2 <- temp
temp <- temp2

str(temp)

temp <- temp %>%
  mutate(Day = lubridate::day(Date)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  mutate(Year = lubridate::year(Date))

temp$StoreType <- as.factor(temp$StoreType)
temp$Date <- NULL
temp$Customers <- NULL
temp$Day <- as.numeric(temp$Day)

temp <- temp[c("Day", "Month", "Year", "Sales")]

temp$Day <- as.integer(temp$Day)
temp$Month <- as.integer(temp$Month)
temp$Year <- as.integer(temp$Year)
temp$Sales <- as.integer(temp$Sales)

#na_count <-sapply(temp, function(y) sum(length(which(is.na(y)))))

train.full.sparse <- sparse.model.matrix(Sales~.-1, data=temp)

dtrain <- xgb.DMatrix(data=train.full.sparse,label=temp$Sales)

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
  nround=50, # number of trees
  verbose=0, # do not show partial info
  nfold=5, # number of CV folds
  feval=rmpse, # custom evaluation metric
  maximize = FALSE # the lower the evaluation score the better
)


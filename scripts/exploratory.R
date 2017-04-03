library(readr)

## Note setup:
#Add your data folder into this directory and
# Read files
rossman.train <- read_csv(file = './data/train.csv')
rossman.test <- read_csv(file = './data/test.csv')
rossman.store <- read_csv(file = './data/store.csv')
#change

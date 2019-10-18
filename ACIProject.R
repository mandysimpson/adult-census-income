library(tidyverse)
library(caret)


adult_census_income <- read.csv("adult.csv")


set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(adult_census_income$income, times = 1, p = 0.2, list = FALSE)
test_set <- adult_census_income[test_index, ]
train_set <- adult_census_income[-test_index, ]
library(tidyverse)
library(caret)
library(dplyr)


adult_census_income <- read.csv("adult.csv")

nrows(adult_census_income)
ncols(adult_census_income)

#drop rows with ? in the data
aci <- filter(adult_census_income, !workclass == "?", !occupation == "?", !native.country == "?")
aci <- droplevels(aci)

#plot fnlwgt against income
aci %>% ggplot(aes(income,fnlwgt)) + geom_boxplot()

#drop fnlwgt and education.num
aci <- aci %>% select(-c(fnlwgt,education.num))

#drop capital.gain, capital.loss and native.country
nearZeroVar(aci)
aci <- aci %>% select(-c(capital.gain, capital.loss, native.country))





set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(adult_census_income$income, times = 1, p = 0.2, list = FALSE)
test_set <- adult_census_income[test_index, ]
train_set <- adult_census_income[-test_index, ]
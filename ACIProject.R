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

#drop fnlwgt and education
aci <- aci %>% select(-c(fnlwgt,education))

#drop capital.gain, capital.loss and native.country
nearZeroVar(aci)
aci <- aci %>% select(-c(capital.gain, capital.loss, native.country))

#plots and other exploration for each attribute
aci %>% ggplot(aes(age, y = ..count.., fill = income)) + geom_density(alpha = 0.6)
aci %>% ggplot(aes(workclass, fill = income)) + geom_bar()
aci %>% ggplot(aes(workclass, fill = income)) + geom_bar(position = "fill") + labs(y = "proportion")
aci %>% ggplot(aes(education.num, y = ..count.., fill = income)) + geom_bar(position = "fill") + labs(y = "proportion")
aci %>% ggplot(aes(marital.status, fill = income)) + geom_bar(position = "fill") + labs(y = "proportion") + scale_x_discrete(labels = abbreviate)
aci %>% ggplot(aes(marital.status, fill = income)) + geom_bar(position = "fill") + labs(y = "proportion") + scale_x_discrete(labels = abbreviate) + facet_grid(sex ~ .)
aci %>% ggplot(aes(occupation, fill = income)) + geom_bar(position = "fill") + labs(y = "proportion") + scale_x_discrete(labels = abbreviate)
aci %>% ggplot(aes(relationship, fill = income)) + geom_bar(position = "fill") + labs(y = "proportion") + scale_x_discrete(labels = abbreviate)
aci %>% filter(relationship == "Wife" | relationship == "Husband") %>% group_by(marital.status) %>% summarise(count = n())

#remove relationship attribute
aci <- aci %>% select(-relationship)

#plots and other exploration for each attribute
aci %>% ggplot(aes(race, fill = income)) + geom_bar(position = "fill") + labs(y = "proportion")
aci %>% ggplot(aes(sex, fill = income)) + geom_bar(position = "fill") + labs(y = "proportion")
aci %>% ggplot(aes(hours.per.week, y = ..count.., fill = income)) + geom_histogram(binwidth = 5, position = "fill") + labs(y = "proportion")


#splitting the dataset for validation and training

set.seed(1,sample.kind = "Rounding")
#if using R3.5 or earlier set.seed(1)

test_index <- createDataPartition(aci$income, times = 1, p = 0.2, list = FALSE)
validation <- aci[test_index, ]
training <- aci[-test_index, ]
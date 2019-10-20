#install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

#download file
adult_census_income <- read.csv("https://github.com/mandysimpson/adult-census-income/raw/master/adult.csv")

#dimensions
nrows(adult_census_income)
ncols(adult_census_income)

#drop rows with ? in the data
aci <- filter(adult_census_income, 
              !workclass == "?", 
              !occupation == "?", 
              !native.country == "?")
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
set.seed(1,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(1)
test_index <- createDataPartition(aci$income, times = 1, p = 0.2, list = FALSE)
validation <- aci[test_index, ]
aci_training <- aci[-test_index, ]

#splitting the dataset again for testing
set.seed(10,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(10)
test_index2 <- createDataPartition(aci_training$income, times = 1, p = 0.2, list = FALSE)
testing <- aci_training[test_index2, ]
training <- aci_training[-test_index2, ]

#knn algorithm
set.seed(3,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(3)
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(income ~ ., 
                   method = "knn", 
                   data = training, 
                   tuneGrid = data.frame(k = seq(5,41,2)), 
                   trControl = control)
ggplot(train_knn,highlight = TRUE)
train_knn$bestTune
confusionMatrix(predict(train_knn, testing, type = "raw"), 
                testing$income)$overall["Accuracy"]

#classification tree algorithm
set.seed(3,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(3)
train_rpart <- train(income ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, len=25)), data = training)
plot(train_rpart)
train_rpart$bestTune
confusionMatrix(predict(train_rpart, testing), testing$income)$overall["Accuracy"]
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

#random forest algorithm
set.seed(3,sample.kind = "Rounding")  #if using R3.5 or earlier set.seed(3)
train_rf <- randomForest(income ~ ., data = training)
confusionMatrix(predict(train_rf, testing), testing$income)$overall["Accuracy"]
importance(train_rf)


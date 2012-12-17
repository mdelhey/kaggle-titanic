# Kaggle - Titanic: Machine Learning from Disaster
# Getting started with Excel in R
library(ggplot2)

train <- read.csv("train.csv", stringsAsFactors = F) # 891 obs
test <- read.csv("test.csv", stringsAsFactors = F)   # 418 obs

# Look at proportion of survival by sex
qplot(factor(survived), data = train, fill = sex) + facet_wrap(~ sex)
male <- train$sex == "male"
female <- train$sex == "female"
lived <- train$survived == "1"
(length(train[male & lived, 1]) / length(train[male, 1])) * 100
(length(train[female & lived, 1]) / length(train[female, 1])) * 100

# prop male survived: 19%; prop female survived: 74%
# We will say that all women are going to live; all men are going to die

# Make the first gender-based prediction
test$survived[test$sex == "female"] <- 1
test$survived[test$sex == "male"] <- 0
write.csv(test, "genderbasedmodel.csv")

# Improve the prediction by considering Age
adult <- train$age > 18
child <- train$age <= 18
train$age.bin[adult] <- "adult"
train$age.bin[child] <- "child"
train$age.bin[!adult & !child] <- NA
qplot(factor(survived), data = train, fill = sex) + facet_wrap(~ age.bin)
length(train[adult & male & lived & !is.na(train$age), 1]) / 
  length(train[adult & male & !is.na(train$age), 1])
length(train[adult & female & lived & !is.na(train$age), 1]) /
  length(train[adult & female & !is.na(train$age), 1])
# Age doesn't give much additional information (than sex) regarding survivability 

# Improve the prediction by considering Passenger Class
qplot(sex, data = train, fill = factor(pclass)) + facet_wrap(~ survived)
qplot(factor(survived), data = train) + facet_wrap(~ pclass)
# Consider Fare price by bining from <10, 10-20, 20-30, >30
bin.1 <- train$fare < 10
bin.2 <- 10 <= train$fare & train$fare < 20
bin.3 <- 20 <= train$fare & train$fare <= 30
bin.4 <- 30 < train$fare
train$fare.bin[bin.1] <- "<10"
train$fare.bin[bin.2] <- "10-20"
train$fare.bin[bin.3] <- "20-30"
train$fare.bin[bin.4] <- ">30"
qplot(sex, data = train, fill = factor(fare.bin)) + facet_wrap(~ survived)
qplot(factor(survived), data = train, fill = factor(fare.bin)) + facet_wrap(~ sex)

length(train[adult & male & lived, 1]) / length(train[adult & male, 1])


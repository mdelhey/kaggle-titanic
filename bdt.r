###
### Boosted Decision Trees
###
library(gbm)
library(plyr)
library(ada)
library(caret)
library(C50)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create data model
###
boosted <- ada(survived ~ sex + pclass + age + fare, data = train)
boosted2 <- ada(survived ~ sex.name + pclass + age + fare + family, 
                data = train, iter = 10000)

## Create C50 model
crtree <- C5.0(survived ~ sex + pclass + age + fare, data = train)

# Save our model as a string
model <- "ada(survived ~ sex.name + pclass + age + fare + family, data = train, iter = 500)"

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(boosted, train)
train$survived_pred2 <- predict(crtree, train)
train$survived_pred3 <- predict(boosted2, train)

# Make our prediction on the TEST data set
test$survived <- predict(boosted, test)
test2 <- test
test2$survived <- predict(crtree, test)
test3 <- test
test3$survived <- predict(boosted2, test)

# save csv file for submission
write.csv(test, "Submissions/bdt-01.csv")
write.csv(test2, "Submissions/bdt-02.csv")
write.csv(test3, "Submissions/bdt-03.csv")

###
### CV
###
#source("5-model_testing.R")
#train_error(survived_pred)
#cv_kfolds(model, k = 9)
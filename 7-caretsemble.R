# Goal: (1) Use caret to do everything

library(caret)
library(randomForest)
library(kernlab)
library(nnet)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

# Lets train a nnet
train(survived ~ sex.name + pclass + age_scale + fare_scale, 
      data = train, method = "nnet")

## size = 5, decay = 1e-04

# Keep training, lets use k fold
netControl <- trainControl(method = "repeatedcv", number = 9,
                           repeats = 20)

train(survived ~ sex.name + pclass + age_scale + fare_scale, 
      data = train, method = "nnet",
      trControl = netControl)

## size = 5, decay = 1e-04

# Keep training, use leave one out
netControl <- trainControl(method = "repeatedcv", number = 10,
                           repeats = 10)

netTrain <- train(survived ~ sex.name + pclass + age_scale + fare_scale, 
            data = train, method = "nnet",
            trControl = netControl)

## Lets try training an ada boost model
adaControl <- trainControl(method = "cv", number = 10)

adaTrain <- train(survived ~ sex.name + pclass + age_scale + fare_scale, 
                  data = train, method = "ada",
                  trControl = adaControl)

## iter = 150, maxdepth = 3, nu = .1

## Now lets try logitBoost but let it preprocess our data its own way
logitControl <- trainControl(method = "repeatedcv", number = 10,
                             repeats = 10)

logitTrain <- train(survived ~ sex.name + pclass + age_scale + fare_scale, 
                    data = train, method = "logitBoost",
                    trControl = logitControl)

# nIter = 50

## Lets use our logit model, it seems good.
testLogit <- test
testLogit <- testLogit[,c(15, 16, 12, 1)]
testLogitpred <- test[,11]
testLogitpred <- as.numeric(testLogitpred)
trainLogit <- train
trainLogit <- trainLogit[,c(15, 16, 12, 1)]
test$survived <- predict(logitTrain, testLogit)

## Lets try rpart
rpartControl <- trainControl(method = "repeatedcv", number = 10,
                             repeats = 10)

rpartTrain <- train(survived ~ sex.name + pclass + age_scale + fare_scale, 
                    data = train, method = "rpart",
                    trControl = rpartControl)

## Random forest
bstControl <- trainControl(method = "repeatedcv", number = 10,
                             repeats = 10)

bstTrain <- train(survived ~ sex.name + pclass + age_scale + fare_scale, 
                    data = train, method = "bstTree",
                    trControl = bstControl)

## Use inhouse feature selection
glmControl <- trainControl(method = "LOOCV")

glmTrain <- train(survived ~ sex + sex.name + pclass + age +
                  age_scale + fare + fare_scale + embarked,
                  data = train, method = "glmboost",
                  trControl = glmControl)

## Lets use our adaboost predictions for shits and giggles
test$survived <- predict(adaTrain, test)
test2 <- test
test2$survived <- predict(netTrain, test2)

## Submit
write.csv(test, "Submissions/caretsemble-01.csv")
write.csv(test2, "Submissions/caretsemble-02.csv")

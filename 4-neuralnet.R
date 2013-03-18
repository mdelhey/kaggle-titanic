<<<<<<< HEAD
# Goal:     (1) Construct basic randomForest models from the data
#           (2) Select the best model (Model selection)
#           (3) Save a prediction with our best randomForest
library(neuralnet)
library(plyr)
library(nnet)
library(caret)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create neural network model
###

## Make vectors because neuralnet is weird
sex <- train$sex
pclass <- train$pclass
fare <- train$fare
age <- train$age
survived <- train$survived

vectors <- cbind(sex, pclass, fare, age, survived)
vectors <- as.data.frame(vectors)
vectors$sex <- factor(vectors$sex)
vectors$pclass <- factor(vectors$pclass)
vectors$survived <- factor(vectors$survived)

sex.test <- test$sex
pclass.test <- test$pclass
fare.test <- test$fare
age.test <- test$age
survived.test <- test$survived

vectors.test <- cbind(sex.test, pclass.test, fare.test, age.test)
vectors.test <- as.data.frame(vectors.test)
colnames(vectors.test) <- c("sex", "pclass", "fare", "age")

vectors.test$sex.test <- factor(vectors.test$sex)
vectors.test$pclass.test <- factor(vectors.test$pclass)
vectors.test$survived.test <- factor(vectors.test$survived)

## Add family column
train$family <- NA
test$family <- NA

train$family[which(train$sibsp != 0 | train$parch != 0)] <- 1
train$family[which(train$sibsp == 0 & train$parch == 0)] <- 0

test$family[which(test$sibsp != 0 | test$parch != 0)] <- 1
test$family[which(test$sibsp == 0 & test$parch == 0)] <- 0

test$family <- factor(test$family)
train$family <- factor(train$family)

# Create neural network based on PCLASS, SEX, and FARE
net <- neuralnet(survived ~ sex + pclass + fare + age, data = vectors,
                 hidden = 2, err.fct="ce")

net2 <- neuralnet(survived ~ sex + pclass + fare + age, data = vectors,
                 hidden = 7, err.fct="ce", linear.output = FALSE)

net3 <- nnet(survived ~ sex + pclass + fare + age, data = train, size = 2,
             linout = FALSE, maxit = 10000)

net4 <- nnet(survived ~ sex.name + pclass + fare + age, data = train, size = 2,
             linout = FALSE, maxit = 10000) 

net5 <- nnet(survived ~ sex.name + pclass + fare + age + family,
             data = train, size = 2, linout = FALSE, maxit = 10000) 

net6 <- nnet(survived ~ sex.name + pclass + fare_scale + age_scale,
             data = train, size = 2, linout = FALSE, maxit = 10000)

model_net <- "nnet(survived ~ sex.name + pclass + fare_scale + age_scale,
             data = train, size = 2, linout = FALSE, maxit = 10000)"

## Get the result
result <- compute(net, vectors.test)
result2 <- compute(net2, vectors.test)
result3 <- predict(net3, test, type = "class")
result4 <- predict(net4, test, type = "class")
result5 <- predict(net5, test, type = "class")
result6 <- predict(net6, test, type = "class")

## Since neuralnet is being a bitch, we round values
result$net.result[which(result$net.result < 1.5)] <- 1
result$net.result[which(result$net.result >= 1.5)] <- 2

test$survived <- result$net.result

test$survived[which(test$survived == 1)] <- 0
test$survived[which(test$survived == 2)] <- 1

## Use result 3
test.net <- test
test.net$survived <- result3

## Use result 4
test.net4 <- test
test.net4$survived <- result4

## Use result 5
test.net5 <- test
test.net5$survived <- result5

## Use result 6
test.net6 <- test
test.net6$survived <- result6

###
### Saving our model and prediction as a new CSV
###

# save csv file for submission
write.csv(test, "Submissions/neuralnet-01.csv")
write.csv(test.net, "Submissions/neuralnet-02.csv")
write.csv(test.net4, "Submissions/neuralnet-03.csv")
write.csv(test.net5, "Submissions/neuralnet-04.csv")
write.csv(test.net6, "Submissions/neuralnet-05.csv")
=======
# Goal:     (1) Construct basic randomForest models from the data
#           (2) Select the best model (Model selection)
#           (3) Save a prediction with our best randomForest
library(neuralnet)
library(plyr)
library(nnet)
library(caret)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create neural network model
###

## Make vectors because neuralnet is weird
sex <- train$sex
pclass <- train$pclass
fare <- train$fare
age <- train$age
survived <- train$survived

vectors <- cbind(sex, pclass, fare, age, survived)
vectors <- as.data.frame(vectors)
vectors$sex <- factor(vectors$sex)
vectors$pclass <- factor(vectors$pclass)
vectors$survived <- factor(vectors$survived)

sex.test <- test$sex
pclass.test <- test$pclass
fare.test <- test$fare
age.test <- test$age
#survived.test <- test$survived

vectors.test <- cbind(sex.test, pclass.test, fare.test, age.test)
vectors.test <- as.data.frame(vectors.test)
colnames(vectors.test) <- c("sex", "pclass", "fare", "age")

vectors.test$sex.test <- factor(vectors.test$sex)
vectors.test$pclass.test <- factor(vectors.test$pclass)
#vectors.test$survived.test <- factor(vectors.test$survived)

# Create neural network based on PCLASS, SEX, and FARE
#net <- neuralnet(survived ~ sex + pclass + fare + age, data = vectors,
#                 hidden = 2, err.fct="ce")

#net2 <- neuralnet(survived ~ sex + pclass + fare + age, data = vectors,
#                 hidden = 7, err.fct="ce", linear.output = FALSE)

net3 <- nnet(survived ~ sex + pclass + fare + age, data = train, size = 2,
             linout = FALSE, maxit = 10000)
model <- "nnet(survived ~ sex + pclass + fare + age, data = train, size = 2, linout = FALSE, maxit = 10000)"

#net4 <- nnet(survived ~ sex.name + pclass + fare + age, data = train, size = 2,
#             linout = FALSE, maxit = 10000) 

## Get the result
#result <- compute(net, vectors.test)
#result2 <- compute(net2, vectors.test)
result3 <- predict(net3, test, type = "class")
#result4 <- predict(net4, test, type = "class")

## Since neuralnet is being a bitch, we round values
#result$net.result[which(result$net.result < 1.5)] <- 1
#result$net.result[which(result$net.result >= 1.5)] <- 2

#test$survived <- result$net.result

#test$survived[which(test$survived == 1)] <- 0
#test$survived[which(test$survived == 2)] <- 1

## Use result 3
test.net <- test
test.net$survived <- result3

## Use result 4
#test.net4 <- test
#test.net4$survived <- result4

###
### Saving our model and prediction as a new CSV
###

# save csv file for submission
write.csv(test, "Submissions/neuralnet-01.csv")
write.csv(test.net, "Submissions/neuralnet-02.csv")
#write.csv(test.net4, "Submissions/neuralnet-03.csv")
>>>>>>> 9184ca6ddd2bae49789cd120e77105a0de946972

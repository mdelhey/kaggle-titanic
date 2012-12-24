# Goal: construct baisc randomForest model (aka baby's first randomForest)
library(randomForest)
library(plyr)

train <- read.csv("train.csv", stringsAsFactors = FALSE)  # 891 obs
test <- read.csv("test.csv", stringsAsFactors = FALSE)    # 418 obs

###
### Clean the data set before running randomForest
### 

# Combine the data sets for age/fare modeling
full <- join(test, train, type = "full")

# Create models for predicting missing values in AGE and FARE
age.mod <- lm(age ~ factor(pclass) + factor(sex) + sibsp + parch + fare, data = full)
fare.mod<- lm(fare ~ factor(pclass) + factor(sex) + sibsp + parch + age, data = full)

# Replace missing values in AGE and FARE with prediction
train$age[is.na(train$age)] <- predict(age.mod, train)
train$fare[is.na(train$fare)] <- predict(fare.mod, train)
test$age[is.na(test$age)] <- predict(age.mod, test)
test$fare[is.na(test$fare)] <- predict(fare.mod, test)

# Replace missing values in embarked with most popular
train$embarked[train$embarked == ""] <- "S"

# Convert to factors
train$sex <- factor(train$sex)
train$embarked <- factor(train$embarked)
train$survived <- factor(train$survived)
test$sex <- factor(test$sex)
test$embarked <- factor(test$embarked)

###
### Create randomForest object and make prediction
###

# Create forest without name, ticket, cabin, or embarked
forest <- randomForest(survived ~ pclass + sex + age + 
            sibsp + parch + embarked, data = train,
            ntree = 10000, importance = TRUE)

# Extract the importance of each variable
importance(forest)

# Check to see how many predictions our forest gets
# correct in the test data set. This gives us a rough 
# estimate of how our model might perform
train$survived_pred <- predict(forest, train, type = "class")
which(train$survived_pred != train$survived)

# Make a prediction with our randomForest
test$survived <- predict(forest, test)

# save csv file for submission
write.csv(test, "second-forest.csv")

###
### Conditional Tree 
###
library(party)

tree <- ctree(survived ~ pclass + sex + age + 
  sibsp + parch + embarked, data = train)

# Check to see how many predictions our forest gets
# correct in the test data set. This gives us a rough 
# estimate of how our model might perform
train$survived_pred <- predict(tree, train)
which(train$survived_pred != train$survived)

logit <- glm(survived ~ pclass + sex + age + sibsp + 
  parch + embarked, family = binomial(logit), data = train)
pr <- predict(logit, train)
pr[pr >= 0] <- 1
pr[pr < 0] <- 0

# Check to see how many predictions our forest gets
# correct in the test data set. This gives us a rough 
# estimate of how our model might perform
train$survived_pred <- pr
which(train$survived_pred != train$survived)

###
### svm
###
library(kernlab)

svm.model <- ksvm(survived ~ pclass + sex + age + 
  sibsp + parch + embarked, data = train)

# Check to see how many predictions our forest gets
# correct in the test data set. This gives us a rough 
# estimate of how our model might perform
train$survived_pred <- predict(svm.model, train, type = "response")
which(train$survived_pred != train$survived)

###
### knn
###
library(class)

knn(train, test, factor(rep(train$survived, 1)))
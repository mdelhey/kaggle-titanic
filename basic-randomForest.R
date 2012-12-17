# baisc randomForest model (aka baby's first randomForest)
library(randomForest)

train <- read.csv("train.csv", stringsAsFactors = F)  # 891 obs
test <- read.csv("test.csv", stringsAsFactors = F)    # 418 obs

###
### Clean the data set before running randomForest
### 

# Replace missing values in AGE and FARE with mean
train$age[is.na(train$age)] <- mean(train$age)
train$fare[is.na(train$fare)] <- mean(train$fare)
test$age[is.na(test$age)] <- mean(test$age)
test$fare[is.na(test$fare)] <- mean(test$fare)

# Convert survived, sex & embarked to factors
train$sex <- factor(train$sex)
train$embarked <- factor(train$sex)
train$survived <- factor(train$survived)
test$sex <- factor(test$sex)
test$embarked <- factor(test$embarked)

###
### Create randomForest object and make prediction
###

# Create forest without name, ticket, cabin, or embarked
forest <- randomForest(factor(survived) ~ . -name -ticket -cabin -embarked,
                       data = train_tidy, 
                       ntree = 5000,
                       importance=TRUE)

# Make a prediction with our randomForest
test$survived <- predict(forest, test, type = "class")

# save csv file for submission
write.csv(test, "babys-first-forest.csv")
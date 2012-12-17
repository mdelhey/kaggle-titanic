# baisc Support vector Machine
library(kernlab)

train <- read.csv("train.csv", stringsAsFactors = F)  # 891 obs
test <- read.csv("test.csv", stringsAsFactors = F)    # 418 obs

###
### Clean the data set before running SVC model
### 

# Remove name, ticket, cabin
train <- train[ , -3] # remove name
train <- train[ , -7] # remove ticket
train <- train[ , -8] # remove cabin

# Replace missing values in AGE and FARE with mean
train$age[is.na(train$age)] <- mean(train$age, na.rm = TRUE)
train$fare[is.na(train$fare)] <- mean(train$fare, na.rm = TRUE)
test$age[is.na(test$age)] <- mean(test$age, na.rm = TRUE)
test$fare[is.na(test$fare)] <- mean(test$fare, na.rm = TRUE)

# Convert survived, sex, pclass, sibsp, parch & embarked to factors
train$survived <- factor(train$survived)
train$sex <- factor(train$sex)
train$pclass <- factor(train$pclass)
train$sibsp <- factor(train$sibsp)
train$parch <- factor(train$parch)
train$embarked <- factor(train$embarked)
test$sex <- factor(test$sex)
test$pclass <- factor(test$pclass)
test$sibsp <- factor(test$sibsp)
test$parch <- factor(test$parch)
test$embarked <- factor(test$embarked)

# Create the SVM model
svm.model <- ksvm(factor(survived) ~ factor(sex) + age + fare, data = train)

# Make a prediction with our SVM model
test$survived <- predict(svm.model, test, type = "response")

# save csv file for submission
write.csv(test, "svm-model-01.csv")
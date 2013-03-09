# Goal:     (1) Construct basic randomForest models from the data
#           (2) Select the best model (Model selection)
#           (3) Save a prediction with our best randomForest
library(RSofia)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create randomForest model
###

# Create random forest based on PCLASS, SEX, FARE, and AGE
train$survived <- as.numeric(train$survived)
train$survived[train$survived == 1] <- -1
train$survived[train$survived == 2] <- 1
sf <- sofia(survived ~ sex + pclass + age + fare, data = train, learner_type="logreg-pegasos")

model <- 'sofia(survived ~ sex + pclass + age + fare, data = train, learner_type="logreg-pegasos")'

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(sf, train, prediction_type = "logistic")

# Make our prediction on the TEST data set
test$survived <- predict(sf, test, prediction_type = "logistic")
test$survived[test$survived >= 0.5] <- 1
test$survived[test$survived < 0.5] <- 0

# save csv file for submission
write.csv(test, "Submissions/randomForest-04.csv")
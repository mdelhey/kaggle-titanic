# Goal: Test the accuracy of our model.
library(plyr)

# Load in our model (edit this!)
source("1-randomForest.csv")

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Training Error
###

# The training error is the error in our model
# on the TRAIN data set. This is usually NOT a 
# good measure of model accuracy on new data.

# Make our prediction on the TRAIN data set
train$survived_pred <- predict(forest, train, type = "class")

# Check to see which predictions our model gets wrong
which(train$survived_pred != train$survived)

# Calculate our % accuracy on the TRAIN data set
((length(which(train$survived_pred == train$survived))) /
  length(train$survived)) * 100

###
### Cross Validation
###

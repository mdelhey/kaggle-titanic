# Goal:     (1) Construct basic randomForest models from the data
#           (2) Select the best model (Model selection)
#           (3) Save a prediction with our best randomForest
library(gbm)
library(plyr)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create randomForest model
###

# Create random forest based on PCLASS, SEX, FARE, AGE
gbm <- gbm(survived ~ sex.name + pclass + fare.distance + age, 
           data = train, distribution = "adaboost", n.trees = 1000)

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(gbm, train, n.trees = 500)

# Make our prediction on the TEST data set
test$survived <- predict(gbm, test, n.trees = 500)

# save csv file for submission
write.csv(test, "Submissions/gbm-01.csv")
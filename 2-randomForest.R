# Goal:     (1) Construct basic randomForest models from the data
#           (2) Select the best model (Model selection)
#           (3) Save a prediction with our best randomForest
library(randomForest)
library(plyr)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create randomForest model
###

# Create random forest based on PCLASS, SEX, FARE, and AGE
forest <- randomForest(survived ~ sex.name + pclass + age + fare + fare.distance, 
                       data = train, ntree = 15000, importance = TRUE)

# Use scaled variables
forest_scale <- randomForest(survived ~ sex.name + pclass + age_scale + fare_scale,
                             data = train, ntree = 15000, importance = TRUE)

summary(forest)

# Extract the importance of each variable
importance(forest)

# Save our model as a string
model <- "randomForest(survived ~ sex.name + pclass + age + fare + fare.distance, data = train, ntree = 5000, importance = TRUE)"
model_scale <- "randomForest(survived ~ sex.name + pclass + age_scale + fare_scale,
                             data = train, ntree = 15000, importance = TRUE)"

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(forest, train)
train$survived_pred2 <- predict(forest_scale, train)

# Make our prediction on the TEST data set
test$survived <- predict(forest, test)
test2 <- test
test2$survived <- predict(forest_scale, test2)

# save csv file for submission
write.csv(test, "Submissions/randomForest-04.csv")
write.csv(test2, "Submissions/randomForest-05-scale.csv")

###
### CV
###
source("5-model_testing.R")
train_error(survived_pred)
cv_kfolds(model, k = 9)
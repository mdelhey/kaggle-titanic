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
forest <- randomForest(survived ~ sex.name + pclass + age + fare.distance + fare, 
                       data = train, ntree = 15000, importance = TRUE)

summary(forest)

# Extract the importance of each variable
importance(forest)

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(forest, train)

# Make our prediction on the TEST data set
test$survived <- predict(forest, test)

# save csv file for submission
write.csv(test, "Submissions/randomForest-04.csv")
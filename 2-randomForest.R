# Goal: (1) Construct basic randomForest models from the data
#       (2) Select the best model (Model selection)
#       (3) Save a prediction with our best randomForest
library(randomForest)
library(plyr)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create randomForest model
###

# Create random forest based on PCLASS, SEX, and FARE
forest <- randomForest(survived ~ pclass + sex + fare, data = train,
                         ntree = 15000, importance = TRUE)

summary(forest)

# Extract the importance of each variable
importance(forest)

###
### Model Selection and Improvement
###

# Check to see how many predictions our forest gets
# correct in the test data set. This gives us a rough 
# estimate of how our model might perform
train$survived_pred <- predict(forest, train, type = "class")
which(train$survived_pred != train$survived)

# Calculate our % accuracy on the train data set
((length(which(train$survived_pred == train$survived))) /
  length(train$survived)) * 100

###
### Saving our model and prediction as a new CSV
###

# Make a prediction with our randomForest
test$survived <- predict(forest, test)

# save csv file for submission
write.csv(test, "Submissions/randomForest.csv")
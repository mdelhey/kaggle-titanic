# Goal:     Boosted descion trees
library(randomForest)
library(plyr)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create randomForest model
###

# Create random forest based on PCLASS, SEX, FARE, and AGE
forest <- randomForest(survived ~ sex + pclass + age + fare + family, 
                       data = train, ntree = 15000, importance = TRUE)

summary(forest)

# Extract the importance of each variable
importance(forest)

# Save our model as a string
model <- "randomForest(survived ~ sex + pclass + age + fare + family, data = train, ntree = 5000, importance = TRUE)"

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(forest, train)

# Make our prediction on the TEST data set
test$survived <- predict(forest, test)

# save csv file for submission
write.csv(test, "Submissions/randomForest-04.csv")
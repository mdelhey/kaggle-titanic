# Goal:     Niave Bayes
library(e1071)
library(plyr)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create randomForest model
###

# Create random forest based on PCLASS, SEX, FARE, and AGE
nb <- naiveBayes(survived ~ sex + pclass + age + fare, data = train)

summary(nb)

# Save our model as a string
model <- "naiveBayes(survived ~ sex + pclass + age + fare, data = train)"

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(nb, train)

# Make our prediction on the TEST data set
test$survived <- predict(nb, test)

# save csv file for submission
write.csv(test, "Submissions/niave-bayes.csv")
# Goal: construct baisc Support vector Machine model
library(kernlab)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Normalize / pre-proccess data
###


###
### Create SVM model
###

# Create the SVM model with SEX, PCLASS, FARE, and AGE
svm.model <- ksvm(survived ~ sex.name + pclass + age + fare + fare.distance, data = train)

# Save our model as a string
model <- "ksvm(survived ~ sex.name + pclass + age + fare + fare.distance, data = train)"

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(svm.model, train, type = "response")

# Make our prediction on the TEST data set
test$survived <- predict(svm.model, test, type = "response")

# save csv file for submission
write.csv(test, "Submissions/svm-model-05.csv")
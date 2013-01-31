# Goal: construct baisc Support vector Machine model
library(kernlab)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create SVM model
###

# Create the SVM model with SEX, AGE, FARE
svm.model <- ksvm(survived ~ sex + age + fare, data = train)

###
### Model Selection and Improvement
###

# Check to see how many predictions our forest gets
# correct in the test data set. This gives us a rough 
# estimate of how our model might perform
train$survived_pred <- predict(svm.model, train, type = "response")
which(train$survived_pred != train$survived)

# Calculate our % accuracy on the train data set
((length(which(train$survived_pred == train$survived))) /
   length(train$survived)) * 100

###
### Saving our model and prediction as a new CSV
###

# Make a prediction with our SVM model
test$survived <- predict(svm.model, test, type = "response")

# save csv file for submission
write.csv(test, "Submissions/svm-model-02.csv")
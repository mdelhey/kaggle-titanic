# Goal: (1) Construct basic Probit models from the data
library(plyr)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create probit model and make prediction
###

# Create forest without name, ticket, cabin, or embarked
probit <- glm(survived ~ sex + pclass + fare + age, data = train,
                family = binomial(link = "probit"))
summary(probit)

# Check to see how many predictions our forest gets
# correct in the test data set. This gives us a rough 
# estimate of how our model might perform
train$survived_pred <- predict(probit, train)
train$survived_pred[train$survived_pred >= 0.5] <- 1
train$survived_pred[train$survived_pred < 0.5] <- 0
which(train$survived_pred != train$survived)

# Calculate our % accuracy on the train data set
((length(which(train$survived_pred == train$survived))) /
   length(train$survived)) * 100

###
### Saving our model and prediction as a new CSV
###

# Make a prediction with our probit
test$survived <- predict(probit, test)
test$survived[test$survived >= 0.5] <- 1
test$survived[test$survived < 0.5] <- 0

# save csv file for submission
write.csv(test, "Submissions/probit.csv")
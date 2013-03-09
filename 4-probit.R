# Goal: (1) Construct basic Probit models from the data
library(plyr)

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

###
### Create probit model and make prediction
###

# Create probit with SEX, PCLASS, FARE, and AGE
probit <- glm(survived ~ sex.name + pclass + age + fare, data = train,
                family = binomial(link = "probit"))
summary(probit)

# Save model as string
model <- 'glm(survived ~ sex.name + pclass + age + fare, data = train, family = binomial(link = "probit"))'

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(probit, train, type = "response")
train$survived_pred[train$survived_pred >= 0.5] <- 1
train$survived_pred[train$survived_pred < 0.5] <- 0

# Make a prediction with our probit on TEST
test$survived <- predict(probit, test, type = "response")
test$survived[test$survived >= 0.5] <- 1
test$survived[test$survived < 0.5] <- 0

# save csv file for submission
write.csv(test, "Submissions/probit-04.csv")
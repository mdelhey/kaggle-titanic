###
### Logit model
###

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

load("Data/test_clean_age.RData")
load("Data/train_clean_age.RData")

###
### Create probit model and make prediction
###

# Create probit with SEX, PCLASS, FARE, and AGE
logit <- glm(survived ~ sex.name + pclass + age + fare + fare.distance, data = train,
              family = binomial(link = "logit"))


logit_age <- glm(survived ~ sex.name + pclass + age + fare_scale + familia,
                 data = train[!(is.na(train$age)), ],
                 family = binomial(link = "logit"))

logit_nage <- glm(survived ~ sex.name + pclass + fare + familia,
                 data = train[(is.na(train$age)), ],
                 family = binomial(link = "logit"))

summary(logit)

# Save model as string
model <- 'glm(survived ~ sex.name + pclass + age + fare, data = train, family = binomial(link = "logit"))'

###
### Saving our model and prediction as a new CSV
###

# Make our prediction on the TRAIN data set [For calculating error]
train$survived_pred <- predict(logit, train, type = "response")
train$survived_pred[train$survived_pred >= 0.5] <- 1
train$survived_pred[train$survived_pred < 0.5] <- 0

# Use the split model
train$survived_pred2 <- NA
train$survived_pred2[!is.na(train$age)] <- predict(logit_age, 
                                                   train[!(is.na(train$age)), ], 
                                                   type = "response")
train$survived_pred2[is.na(train$age)] <- predict(logit_nage, 
                                                  train[(is.na(train$age)), ], 
                                                   type = "response")

train$survived_pred2[train$survived_pred2 >= 0.5] <- 1
train$survived_pred2[train$survived_pred2 < 0.5] <- 0

# Make a prediction with our probit on TEST
test$survived <- predict(logit, test, type = "response")
test$survived[test$survived >= 0.5] <- 1
test$survived[test$survived < 0.5] <- 0

# Make split prediction
test$survived <- NA
test$survived[!is.na(test$age)] <- predict(logit_age, 
                                           test[!(is.na(test$age)), ], 
                                           type = "response")

test$survived[is.na(test$age)] <- predict(logit_nage, 
                                            test[(is.na(test$age)), ], 
                                            type = "response")
test$survived[test$survived >= 0.5] <- 1
test$survived[test$survived < 0.5] <- 0

# save csv file for submission
write.csv(test, "Submissions/logit-01.csv")
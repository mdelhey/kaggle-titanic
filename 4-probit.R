# Goal: construct baisc probit model
library(plyr)

###
### Create probit model and make prediction
###

# Create forest without name, ticket, cabin, or embarked
probit <- glm(survived ~ pclass + sex + age + sibsp + parch,
              data = train, family = binomial(link = "probit"))

# Check to see how many predictions our forest gets
# correct in the test data set. This gives us a rough 
# estimate of how our model might perform
train$survived_pred <- predict(probit, train)
which(train$survived_pred != train$survived)

# Make a prediction with our randomForest
#test$survived <- predict(forest, test)

# save csv file for submission
#write.csv(test, "probit.csv")
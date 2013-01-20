# Goal: (1) Clean the data sets
#           - fix missing values
#           - fix data structures
#           - save new data sets for future analysis
# Output: (1) Saves two new files for all further analysis: 
#           - test_clean.RData 
#           - train_clean.RData
#         (2) Also saves three csv files for archive purposes
#           - train_clean.csv
#           - test_clean.csv
#           - full.csv
library(plyr)

# Load the data sets
train <- read.csv("Data/train.csv", stringsAsFactors = FALSE)  # 891 obs
test <- read.csv("Data/test.csv", stringsAsFactors = FALSE)    # 418 obs

###
### Fixing missing values
###

# Combine the data sets for age/fare modeling
full <- join(test, train, type = "full")

# Create models for predicting missing values in AGE and FARE
age.mod <- lm(age ~ factor(pclass) + factor(sex) +
                sibsp + parch + fare, data = full)
fare.mod<- lm(fare ~ factor(pclass) + factor(sex) +
                sibsp + parch + age, data = full)

# Replace missing values in AGE and FARE with prediction
train$age[is.na(train$age)] <- predict(age.mod, train)
train$fare[is.na(train$fare)] <- predict(fare.mod, train)
test$age[is.na(test$age)] <- predict(age.mod, test)
test$fare[is.na(test$fare)] <- predict(fare.mod, test)

# Replace missing values in embarked with most popular
train$embarked[train$embarked == ""] <- "S"

###
### Data structures
###

# Create a survived variable in the test data set
# Set "0" (did not survive) as the default value
test$survived <- 0

# Convert catagorical variables to factors
train$survived <- factor(train$survived)
train$sex <- factor(train$sex)
train$pclass <- factor(train$pclass)
train$embarked <- factor(train$embarked)
test$survived <- factor(test$survived)
test$sex <- factor(test$sex)
test$pclass <- factor(test$pclass)
test$embarked <- factor(test$embarked)

###
### Saving new data sets
###

# Save files as RData in order to preserve data structures
# Open .RData with load()
save("test", file = "Data/test_clean.RData")
save("train", file = "Data/train_clean.RData")

# Also save .csv's just in case. These do not preserve data structures,
# so don't use them in the analysis! 
write.csv(test, "Data/CSV/test_clean.csv", row.names = FALSE)
write.csv(train, "Data/CSV/train_clean.csv", row.names = FALSE)
write.csv(full, "Data/CSV/full.csv", row.names = FALSE)
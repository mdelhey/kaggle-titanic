# Goal:         (1) Fix missing values
#               (2) Fix data structures
#               (3) Save new cleaned data sets
#
# Output:       (1) R datasets (maintains data structure)
#                   - test_clean.RData 
#                   - train_clean.RData
#               (2) CSV datasets (archival)
#                   - train_clean.csv
#                   - test_clean.csv
#                   - full.csv
library(plyr)

# Load the data sets
train <- read.csv("Data/train.csv", stringsAsFactors = FALSE)  # 891 obs
test <- read.csv("Data/test.csv", stringsAsFactors = FALSE)    # 418 obs


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
### Fixing missing values
###

# Combine the data sets for age/fare modeling
full <- join(test, train, type = "full")

# Create models for predicting missing values in AGE and FARE
age.mod <- lm(age ~ pclass + sex +
                sibsp + parch + fare, data = full)
fare.mod<- lm(fare ~ pclass + sex +
                sibsp + parch + age, data = full)

# Replace missing values in AGE and FARE with prediction
train$age[is.na(train$age)] <- predict(age.mod, train)
train$fare[is.na(train$fare)] <- predict(fare.mod, train)
test$age[is.na(test$age)] <- predict(age.mod, test)
test$fare[is.na(test$fare)] <- predict(fare.mod, test)

# Replace missing values in embarked with most popular
train$embarked[train$embarked == ""] <- "S"


###
### Create fare-distance metric
###

# fare-distance = fare - mean(fare of pclass)
# Are those who pay less than the average for a ticket less likely to survive?

# Create fare-distance metric for Train
#new <- ddply(train, "pclass", transform, fare_avg = mean(fare))
#new2 <- transform(new, fare_distance = fare - fare_avg)
#train <- new2[, -12]

# Create fare-distance metric for Test
#new <- ddply(test, "pclass", transform, fare_avg = mean(fare))
#new2 <- transform(new, fare_distance = fare - fare_avg)
#test <- new2[, -11]

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

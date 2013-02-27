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

# Remove NA's in AGE and FARE
full.age <- full[!is.na(full$age), ]
full.fare <- full[!is.na(full$fare), ]

# Create LM models for predicting missing values in AGE and FARE
age.mod <- lm(age ~ pclass + sex +
                sibsp + parch + fare, data = full)
fare.mod<- lm(fare ~ pclass + sex +
                sibsp + parch + age, data = full)

# Create RF models for predicting missing values in AGE and FARE
#age.rf <- randomForest(age ~ pclass + sex + sibsp + parch + fare + embarked, data = full.age, 
                       ntree = 1000, importance = TRUE)

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

# Find the mean fare for each pclass
class1 <- subset(full, pclass == 1)
class2 <- subset(full, pclass == 2)
class3 <- subset(full, pclass == 3)
fare1 <- mean(class1$fare, na.rm = TRUE)
fare2 <- mean(class2$fare, na.rm = TRUE)
fare3 <- mean(class3$fare, na.rm = TRUE)

# Create fare_avg column
train$fare_avg[train$pclass == 1] <- fare1
train$fare_avg[train$pclass == 2] <- fare2
train$fare_avg[train$pclass == 3] <- fare3
test$fare_avg[test$pclass == 1] <- fare1
test$fare_avg[test$pclass == 2] <- fare2
test$fare_avg[test$pclass == 3] <- fare3

# Create fare-distance metric for Train
train <- transform(train, fare_distance = fare - fare_avg)
train <- train[, !names(train) %in% c("fare_avg")]

# Create fare-distance metric for Test
test <- transform(test, fare_distance = fare - fare_avg)
test <- test[, !names(test) %in% c("fare_avg")]


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
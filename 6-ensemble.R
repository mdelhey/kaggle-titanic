# Goal: (1) Combine our three models into one prediction:
#             - randomForest
#             - SVM
#             - probit

# Source our data and clean it
source("1-clean.R")

# Source our models
source("2-randomForest.R")
source("3-SVM.R")
source("4-probit.R")

###
### Gather predictions
###

# randomForest
test$survived_rf <- predict(forest, test)

# SVM
test$survived_svm <- predict(svm.model, test, type = "response")

# Probit
test$survived_probit <- predict(probit, test)
test$survived_probit[test$survived >= .5] <- 2
test$survived_probit[test$survived < .5] <- 1

###
### Combine Predictions
###

vote <- as.numeric(test$survived_rf) + 
        as.numeric(test$survived_svm) +
        as.numeric(test$survived_probit)

# 0 is 0 
# 4 is 0
# 5 is 1
# 6 is 1

combined <- vote
combined[combined <= 4] <- 0
combined[combined >= 5] <- 1

# Make our ensamble prediction
test$survived <- combined

write.csv(test, "Submissions/ensemble-08.csv")

# Test to see if our predictions match the highest
highest <- read.csv("Submissions/highest.csv")
which(highest$survived != test$survived)
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
test$

write.csv(test, "Submissions/ensemble-11-upload-me.csv")

# Compare to highest
highest <- read.csv("Submissions/highest.csv")
which(test$survived != highest$survived)
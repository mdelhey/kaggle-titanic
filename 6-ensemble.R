# Goal: (1) Combine our three models into one prediction:
#             - randomForest
#             - SVM
#             - probit

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
test$survived_probit <- predict(probit, test, type = "response")
test$survived_probit[test$survived >= 0.5] <- 1
test$survived_probit[test$survived < 0.5] <- 0

# gbm
#test$survived_gbm <- predict(gbm, test, n.trees = 500)
#test$survived_gbm[test$survived_gbm >= 1] <- 1
#test$survived_gbm[test$survived_gbm < 1] <- 0

###
### Combine Predictions
###

combined <- as.numeric(test$survived_rf) + 
            as.numeric(test$survived_svm) +
            as.numeric(test$survived_probit)

# 3 is 0 
# 4 is 0
# 5 is 1
# 6 is 1

combined[combined <= 4] <- 0
combined[combined >= 5] <- 1

# Make our ensamble prediction
test$survived <- combined

write.csv(test, "Submissions/ensemble-05.csv")
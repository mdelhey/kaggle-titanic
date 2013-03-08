# Goal: Test the accuracy of our model.

# Load in the cleaned data sets
load("Data/train_clean.RData")  # 891 obs
load("Data/test_clean.RData")   # 418 obs

# Load in our models (edit this!)
#source("2-randomForest.R")
#source("3-SVM.R")
#source("4-probit.R")

###
### Training Error
###

# The training error is the error in our model
# on the TRAIN data set. This is usually NOT a 
# good measure of model accuracy on new data.

# Assumes that train$survived_pred has already been created

# Check to see which predictions our model gets wrong
which(train$survived_pred != train$survived)

# Calculate our % accuracy on the TRAIN data set
((length(which(train$survived_pred == train$survived))) /
  length(train$survived)) * 100

###
### Cross Validation
###

# The basic idea is that we randomly divide the original data set into k 
# equally sized chunks. We then perform k training and test runs. In each
# run, we omit one of the chunks from the training set and instead use it as
# the test set. Finally, we average the generalization errors from all
# runs to obtain the overall expected generalization error.

# Set number of equally-sized, non-overlapping chunks
k <- 11

# Find the size of each sampled chunk
k_size <- floor(nrow(train) / k)

# Randomly sample our data set then split it up into row-unique k-chunks
k_sample <- split(sample(nrow(train)), rep(1:(nrow(train)/k_size)))

# Create an errors vector to hold our errors for each test
errors <- 0

# Perform k runs
for (i in 1:k) {
  # Take one k-sample and make it the new train data set
  train_new <- train[-k_sample[[i]], ]
  test_new <- train[k_sample[[i]], ]
  # Train our model on the train_new data set
  
  # When we change our model, we need to edit this!!!
  temp_model <- randomForest(survived ~ sex + pclass + fare, data = train_new)
  
  # Predict on the new_test data set with our model
  test_new$survived_pred <- predict(temp_model, test_new)
  # Find the error in our model for new_test
  errors[i] <- ((length(which(test_new$survived_pred == test_new$survived))) /
     length(test_new$survived)) * 100
}

# Find the average error
mean(errors)

###
### Leave-one-out
###

# Special case of cross validation where k = n, where n = # of data points. 

###
### 0.632 Bootstrap
###
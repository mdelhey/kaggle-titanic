# Getting started with random forests -- in R
library(randomForest)
library(ggplot2)

train <- read.csv("train.csv", stringsAsFactors = F)  # 891 obs
test <- read.csv("test.csv", stringsAsFactors = F)    # 418 obs

# Create tidy data set for runnig randomForest
train_tidy <- train 

# Average age is 30, replace NA's with 30
train_tidy$age[is.na(train_tidy$age)] <- 30 

randomForest(survived ~ . -name, data = train_tidy)
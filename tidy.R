# Goal: Cleaning the data sets
library(ggplot2)

train <- read.csv("train.csv", stringsAsFactors = F)  # 891 obs
test <- read.csv("test.csv", stringsAsFactors = F)    # 418 obs

# 
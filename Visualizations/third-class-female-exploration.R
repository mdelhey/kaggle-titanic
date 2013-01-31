# Goal: Exploratory data analysis to answer the question: Why are women in
#       third class who paid more for their ticket more likely not to survive?
library(ggplot2)

setwd("C:/users/matt/kaggle-titanic/Data/")
train <- read.csv("train.csv", stringsAsFactors = F) # 891 obs
test <- read.csv("test.csv", stringsAsFactors = F)   # 418 obs

# Consider fare by bining from <10, 10-20, 20-30, >30
bin.1 <- train$fare < 10
bin.2 <- 10 <= train$fare & train$fare < 20
bin.3 <- 20 <= train$fare & train$fare <= 30
bin.4 <- 30 < train$fare
train$fare.bin[bin.1] <- "<10"
train$fare.bin[bin.2] <- "10-20"
train$fare.bin[bin.3] <- "20-30"
train$fare.bin[bin.4] <- ">30"
train$fare.bin <- factor(train$fare.bin, levels = c("<10", "10-20", "20-30", ">30"))

# Notice that most females who died did so in 3rd class
qplot(factor(survived), data = train, fill = sex) + facet_wrap(~ pclass)

# Subset for third class
third <- subset(train, pclass == "3")
# Notice that females in third class >=20 died the most
qplot(factor(survived), data = third, fill = sex) + facet_wrap(~ fare.bin)

# Subset for these women
weird <- subset(train, pclass == "3" & fare >= 20 & sex == "female") # 37 obs
# These women have significantly higher parch and
# sibsp values, even compared to all women who died.
# Mean age of weird women is 22, but also a lot of NA's

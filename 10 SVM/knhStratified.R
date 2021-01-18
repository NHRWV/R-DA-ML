library(caTools)

data_set <- read.csv("knhData.csv", header = T)

set.seed(123)

train <- sample.split(data_set$Z, SplitRatio = 0.5)

training <- data_set[train, ]
remainder <- data_set[!train, ]

validate <- sample.split(remainder$Z, SplitRatio = 0.6)

validation <- remainder[validate, ]
test <- remainder[!validate, ]

training
validation
test

setwd("C:/Users/Namlister/Desktop/10 SVM")
rm(list = ls())

D.data <- read.csv('bCancerWC.csv', header = TRUE)
#install.packages('caTools')
#install.packages('e1071')
library(caTools)

pSeed <- 12345
set.seed(pSeed)

train <- sample.split(D.data$Class, SplitRatio = 0.6)

training <- D.data[train, ]
remainder <- D.data[!train, ]

validate <- sample.split(remainder$Class, SplitRatio = 0.5)

validation <- remainder[validate, ]
test <- remainder[!validate, ]

Tra <- training
Val <- validation
Tes <- test

D.tra <- Tra[, -c(1)]
D.val <- Val[, -c(1)]
D.tes <- Tes[, -c(1)]

x <- subset(D.tra, select = -c(Class))
y <- D.tra$Class

library(e1071)

linear_tune <- tune(svm, train.x = x, train.y = y, kernel = 'linear', ranges = list(cost = c(50, 100, 150)))
print(linear_tune) #100일 때 제일 좋았대.

linearSVM <- svm(Class~., D.tra, type = 'C-classification', kernel = 'linear', cost = 50, scale = FALSE)


Predict.tra <- predict(linearSVM, D.tra)
cTab <- table(Actual = D.tra$Class, Predict.tra)
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra

Predict.val <- predict(linearSVM, D.val)
cTab <- table(Actual = D.val$Class, Predict.val)
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val


set.seed(pSeed)
radial_tune <- tune(svm, train.x = x, train.y = y, kernel = 'radial', ranges = list(cost = c(50, 100, 150),
                                                                                    gamma = seq(0.1,1, by = 0.1)))
print(radial_tune) 

modelSVM <- svm(Class~., D.tra, type = 'C-classification', kernel = 'radial', cost = 50, gamma = 0.3, scale = FALSE)


set.seed(pSeed)
sigmoid_tune <- tune(svm, train.x = x, train.y = y, kernel = 'sigmoid', ranges = list(cost = c(50, 100, 150),
                                                                                      coef0 = c(0, 1, 2),
                                                                                      gamma = seq(0.1,1, by = 0.1)))
print(sigmoid_tune)

modelSVM <- svm(Class~., D.tra, type = 'C-classification', kernel = 'sigmoid', cost = 50, coef0 = 2, gamma = 0.1, 
                scale = FALSE)









pSeedlist <- c(12345, 12346, 12347, 12348, 12349, 12350, 12351, 12352, 12353, 12354)

for (pSeed in pSeedlist){
  set.seed(pSeed)
  
  train <- sample.split(D.data$Class, SplitRatio = 0.6)
  
  training <- D.data[train, ]
  remainder <- D.data[!train, ]
  
  validate <- sample.split(remainder$Class, SplitRatio = 0.5)
  
  validation <- remainder[validate, ]
  test <- remainder[!validate, ]
  
  Tra <- training
  Val <- validation
  Tes <- test
  
  D.tra <- Tra[, -c(1)]
  D.val <- Val[, -c(1)]
  D.tes <- Tes[, -c(1)]
  
  x <- subset(D.tra, select = -c(Class))
  y <- D.tra$Class
  
  
  
  
  cat("\n\n", pSeed, "\n")
  modelSVM <- svm(Class~., D.tra, type = 'C-classification', kernel = 'linear', cost = 50, scale = FALSE)
  
  Predict.tra <- predict(modelSVM, D.tra)
  cTab <- table(Actual = D.tra$Class, Predict.tra)
  Accu.tra <- sum(diag(cTab))/sum(cTab)*100
  
  Predict.val <- predict(modelSVM, D.val)
  cTab <- table(Actual = D.val$Class, Predict.val)
  Accu.val <- sum(diag(cTab))/sum(cTab)*100
  
  cat('linear \n')
  print(Accu.tra)
  print(Accu.val)
  
  
  
  set.seed(pSeed)
  modelSVM <- svm(Class~., D.tra, type = 'C-classification', kernel = 'radial', cost = 50, gamma = 0.3, scale = FALSE)
  
  Predict.tra <- predict(modelSVM, D.tra)
  cTab <- table(Actual = D.tra$Class, Predict.tra)
  Accu.tra <- sum(diag(cTab))/sum(cTab)*100
 
  Predict.val <- predict(modelSVM, D.val)
  cTab <- table(Actual = D.val$Class, Predict.val)
  Accu.val <- sum(diag(cTab))/sum(cTab)*100
  
  cat('radial \n')
  print(Accu.tra)
  print(Accu.val)
  
  
  
  set.seed(pSeed)
  modelSVM <- svm(Class~., D.tra, type = 'C-classification', kernel = 'sigmoid', cost = 50, coef0 = 2, gamma = 0.1, 
                  scale = FALSE)
  Predict.tra <- predict(modelSVM, D.tra)
  cTab <- table(Actual = D.tra$Class, Predict.tra)
  Accu.tra <- sum(diag(cTab))/sum(cTab)*100
 
  Predict.val <- predict(modelSVM, D.val)
  cTab <- table(Actual = D.val$Class, Predict.val)
  Accu.val <- sum(diag(cTab))/sum(cTab)*100
  
  cat('sigmoid \n')
  print(Accu.tra)
  print(Accu.val)
  
  
}



for (pSeed in pSeedlist){
  set.seed(pSeed)
  
  train <- sample.split(D.data$Class, SplitRatio = 0.6)
  
  training <- D.data[train, ]
  remainder <- D.data[!train, ]
  
  validate <- sample.split(remainder$Class, SplitRatio = 0.5)
  
  validation <- remainder[validate, ]
  test <- remainder[!validate, ]
  
  Tra <- training
  Val <- validation
  Tes <- test
  
  D.tra <- Tra[, -c(1)]
  D.val <- Val[, -c(1)]
  D.tes <- Tes[, -c(1)]
  
  x <- subset(D.tra, select = -c(Class))
  y <- D.tra$Class
  
  
  cat("\n\n", pSeed, "\n")
  modelSVM <- svm(Class~., D.tra, type = 'C-classification', kernel = 'linear', cost = 50, scale = FALSE)
  
  Predict.tes <- predict(modelSVM, D.tes)
  cTab <- table(Actual = D.tes$Class, Predict.tes)
  Accu.tes <- sum(diag(cTab))/sum(cTab)*100
  
  
  
  cat('linear \n')
  print(Accu.tes)
  
}

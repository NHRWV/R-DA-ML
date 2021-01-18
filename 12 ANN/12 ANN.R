#threshold는 오차의 감소치가 얼마일 때 종료하는지 설정함...너무 작으면 답을 못 구한다....과대적합이기 때문.
#learningrate는 학습률인데 이 또한 너무 작거나 크면 안된다.
#err.fct는 sse, ec 같은 거...여기선 ce 사용한다....act.fct는 활성함수이고 여기선 logistic 사용하자.
#linerar.output...지금 이건 분류문제니까 false로 설정. knhANN...ANN Excel (Virus 없음). 프린트는 2장, 엑셀파일은 한 장.

setwd('C:/Users/Namlister/Desktop/R DA-ML/12 ANN')
rm(list = ls())
Tra <- read.csv('Tra1.csv', header = TRUE)
Val <- read.csv('Val1.csv', header = TRUE)
Tes <- read.csv('Tes1.csv', header = TRUE)

D.tra <- Tra[, -c(1)]
D.val <- Val[, -c(1)]
D.tes <- Tes[, -c(1)]

#install.packages('neuralnet')
library(neuralnet)
wSeed <- 13579


D.tra$Class <- ifelse(D.tra$Class == 2, 0, 1)
D.val$Class <- ifelse(D.val$Class == 2, 0, 1)#이진분류 이렇게 설정 안해주면 오류남...

H42 <- c(4,2)
Thres <- 0.1
Step <- 100000
Rate <- 0.01

set.seed(wSeed)
BPnn <- neuralnet(Class~., D.tra, hidden = H42, threshold = Thres, stepmax = Step, learningrate = Rate,
                  algorithm = 'backprop', err.fct = 'ce', act.fct = 'logistic', linear.output = FALSE) #2는 0으로 4는 1로 바꿔줘야 에러 안남.

plot(BPnn)

cutoff <- 0.5
Output.tra <- predict(BPnn, D.tra)
head(Output.tra)
Predict.tra <- ifelse(Output.tra >= cutoff, 1, 0)
cTab <- table(Actual = D.tra$Class, Predict.tra)
Accu.tra <- round(sum(diag(cTab))/sum(cTab)*100, digits = 2)
Accu.tra


Output.val <- predict(BPnn, D.val)
Predict.val <- ifelse(Output.val >= cutoff, 1, 0)
cTab <- table(Actual = D.val$Class, Predict.val)
Accu.val <- round(sum(diag(cTab))/sum(cTab)*100, digits = 2)
Accu.val

cat(sprintf('\n Thres = %4.2f Seed=%5d', Thres, wSeed))
cat(' Tra =', Accu.tra)
cat(' Val =', Accu.val)

###############################################assignment############################
install.packages('caTools')
install.packages('e1071')
install.packages('neuralnet')
library(caTools)
library(e1071)
library(neuralnet)

wSeedlist <- c(10000, 11000, 50000, 12222, 14000, 15000, 12362, 17000, 12341, 71000)

D.data <- read.csv('bCancerWC.csv', header = TRUE)

for (wSeed in wSeedlist){
  set.seed(wSeed)
  
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
  
  
  D.tes$Class <- ifelse(D.tes$Class == 2, 0, 1)
  D.tra$Class <- ifelse(D.tra$Class == 2, 0, 1)
  D.val$Class <- ifelse(D.val$Class == 2, 0, 1)#이진분류 이렇게 설정 안해주면 오류남...
  
  H42 <- c(6,3)
  Thres <- 0.5
  Step <- 100000
  Rate <- 0.01
  
  
  BPnn <- neuralnet(Class~., D.tra, hidden = H42, threshold = Thres, stepmax = Step, learningrate = Rate,
                    algorithm = 'backprop', err.fct = 'ce', act.fct = 'logistic', linear.output = FALSE)
  
  cutoff <- 0.5
  Output.tra <- predict(BPnn, D.tra)
  head(Output.tra)
  Predict.tra <- ifelse(Output.tra >= cutoff, 1, 0)
  cTab <- table(Actual = D.tra$Class, Predict.tra)
  Accu.tra <- round(sum(diag(cTab))/sum(cTab)*100, digits = 2)
  
  Output.val <- predict(BPnn, D.val)
  Predict.val <- ifelse(Output.val >= cutoff, 1, 0)
  cTab <- table(Actual = D.val$Class, Predict.val)
  Accu.val <- round(sum(diag(cTab))/sum(cTab)*100, digits = 2)
  
  
  cat(sprintf('\n Thres = %4.2f Seed=%5d', Thres, wSeed))
  #cat(' Thres', Thres)
  #cat(' Seed', wSeed)
  cat(' Tra =', Accu.tra)
  cat(' Val =', Accu.val)
  
  Output.tes <- predict(BPnn, D.tes)
  Predict.tes <- ifelse(Output.tes >= cutoff, 1, 0)
  cTab <- table(Actual = D.tes$Class, Predict.tes)
  Accu.tes <- sum(diag(cTab))/sum(cTab)*100
  cat(' Tes =', Accu.tes)
 
  
}

#################TEST DATA SET 적용..?

for (wSeed in wSeedlist){
  set.seed(wSeed)
  
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
  
  
  D.tra$Class <- ifelse(D.tra$Class == 2, 0, 1)
  D.tes$Class <- ifelse(D.tes$Class == 2, 0, 1)
  D.val$Class <- ifelse(D.val$Class == 2, 0, 1)#이진분류 이렇게 설정 안해주면 오류남...
  
  H42 <- c(6,3)
  Thres <- 0.01
  Step <- 500000
  Rate <- 0.01
  
  
  BPnn <- neuralnet(Class~., D.tra, hidden = H42, threshold = Thres, stepmax = Step, learningrate = Rate,
                    algorithm = 'backprop', err.fct = 'ce', act.fct = 'logistic', linear.output = FALSE)
  
  cutoff <- 0.5
  Output.tes <- predict(BPnn, D.tes)
  Predict.tes <- ifelse(Output.val >= cutoff, 1, 0)
  cTab <- table(Actual = D.tes$Class, Predict.tes)
  Accu.tes <- sum(diag(cTab))/sum(cTab)*100
  
  
  
  
  cat(sprintf('\n Thres = %4.2f Seed=%5d', Thres, wSeed))
  #cat(' Thres', Thres)
  #cat(' Seed', wSeed)
  cat(' Tes =', Accu.tes)
  
}

str(test)

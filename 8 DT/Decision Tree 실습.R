setwd("C:/Users/ebiz/Desktop/8 DT")
rm(list = ls())

D.data <- read.csv('HeartD.csv', header = TRUE)
str(D.data)
table(D.data$Class)

Tra <- read.csv('Tra1.csv', header = TRUE)
Val <- read.csv('Val1.csv', header = TRUE)
Tes <- read.csv('Tes1.csv', header = TRUE)

head(Tra)
D.tra <- Tra[, -c(1)]
D.val <- Val[, -c(1)]
D.tes <- Tes[, -c(1)]

#install.packages("rpart")
library(rpart)

set.seed(12345)
e_DT <- rpart(Class ~., D.tra, method = 'class', parms = list(split = 'information')) #엔트로피 불순도 방식.

plot(e_DT, margin = 0.2)
text(e_DT)
e_DT #BC의 뜻...T3은 왼쪽으로, T6과 7은 오른쪽으로 분류한다.

Predict.tra <- predict(e_DT, D.tra, type = 'class')
Actual.tra <- D.tra$Class #실제값
head(Predict.tra)
head(Actual.tra)

#install.packages('caret')
#install.packages('e1071')
library(caret)
library(e1071)

cM1 <- confusionMatrix(Predict.tra, Actual.tra, positive = 'Disease') # 예측 데이터를 먼저 입력.
cM1

cTab <- table(Actual.tra, Predict.tra)
cTab #그림은 이렇게 그리고 정확도 민감도 특이도는 컨퓨전매트릭스로 알아온다.

#또는 이런 방법도 있다!########
Predict.tra <- predict(e_DT, D.tra, type = 'class')
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra, Predict.tra)
cM.tra <- confusionMatrix(t(cTab), positive = 'Disease')
Accu.tra <- cM.tra$overall['Accuracy']
cTab
Accu.tra*100
#####################

Predict.val <- predict(e_DT, D.val, type = 'class')
Actual.val <- D.val$Class
cTab <- table(Actual.val, Predict.val)
cM.val <- confusionMatrix(t(cTab), positive = 'Disease')
Accu.val <- cM.val$overall['Accuracy']
cTab
Accu.val*100

#가지치기 준비
printcp(e_DT) #XERROR가 가장 작을 때 값을 가져와야 한다.
cpval <- e_DT$cptable[which.min(e_DT$cptable[, 'xerror']), 'CP']
cpval

pruned.e_DT <- prune(e_DT, cp = cpval)
plot(pruned.e_DT, margin = 0.2)

#가지치기 한 걸로 시도해본다.
Predict.tra <- predict(pruned.e_DT, D.tra, type = 'class')
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra, Predict.tra)
cM.tra <- confusionMatrix(t(cTab), positive = 'Disease')
Accu.tra <- cM.tra$overall['Accuracy']
cTab
Accu.tra*100

Predict.val <- predict(pruned.e_DT, D.val, type = 'class')
Actual.val <- D.val$Class
cTab <- table(Actual.val, Predict.val)
cM.val <- confusionMatrix(t(cTab), positive = 'Disease')
Accu.val <- cM.val$overall['Accuracy']
cTab
Accu.val*100
# train과 val의 정확도 차이가 줄었다...오버피팅 감소.


g_DT <- rpart(Class ~., D.tra, method = 'class', parms = list(split = 'gini')) #이거는 지니 불순도 방식이다.
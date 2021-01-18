setwd("C:/Users/ebiz/Desktop/9 RF")
#rm(list = ls)

Tra <- read.csv('Tra1.csv', header = TRUE)
val <- read.csv('val1.csv', header = TRUE)
Tes <- read.csv('Tes1.csv', header = TRUE)

R.tra <- Tra[, -c(1)]
R.val <- val[, -c(1)]
R.tes <- Tes[, -c(1)]

#install.packages('randomForest')
library(randomForest)

x <- subset(R.tra, select = -c(Class))
y <- R.tra$Class

mtrySeed <- 1234
set.seed(mtrySeed)
# 랜덤포레스트 튜닝...적정인자값 찾기.
# stepFactor : at each iteration, mtry is inflated (or deflated) by this value
# improve : the (relative) improvement in OOB error must be by this much for the search to continue
bestmtry <- tuneRF(x, y, stepFactor = 1.5, improve = 0.01, ntreeTry = 49)
print(bestmtry)

nTree <- 49
mTry <- 4
rfseed <- 12345
set.seed(rfseed)

rfModel <- randomForest(x, y, ntree = nTree, mtry = mTry) #이미 x와 y로 나눴기에 알아서 형식에 따라 메서드를 정한다.
#그러니까 class를 정할 필요가 없다.
Predict.tra <- predict(rfModel, R.tra)
cTab <- table(Actual = R.tra$Class, Predict.tra)
cTab #100%
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra

# training 끝...val 시작.
# Predict.val <- predict(rfModel, R.val) <-- 이거 에러난다..
# head(R.val) 해보면 CP값에서 P1,2가 없는 경우가 있기에 val데이터에선 R1이 없어서 생기는 일. 부츠트랩 방법 써서 이래.

levels(R.val$CP) <- levels(R.tra$CP)
levels(R.val$Restecg) <- levels(R.tra$Restecg)
levels(R.val$Slope) <- levels(R.tra$Slope)
levels(R.val$Thal) <- levels(R.tra$Thal)

Predict.val <- predict(rfModel, R.val)
cTab <- table(Actual = R.val$Class, Predict.val)
Accu.val <- sum(diag(cTab))/sum(cTab)*100

Variance <- abs(Accu.tra - Accu.val)

Accu.tra
Accu.val
Variance

# overfitting 제거하려면 랜덤트리가 너무 풍성하게 자라지 않게 해야함...따라서 깊이 제한한다. 
# leaf node 개수를 제한함으로써 깊이를 제한한다.

maxLeaf <- 10
rfModel <- randomForest(x, y, ntree = nTree, mtry = mTry, maxnodes = maxLeaf)

Predict.tra <- predict(rfModel, R.tra)
cTab <- table(Actual = R.tra$Class, Predict.tra)
Accu.tra <- sum(diag(cTab))/sum(cTab)*100

Predict.val <- predict(rfModel, R.val)
cTab <- table(Actual = R.val$Class, Predict.val)
Accu.val <- sum(diag(cTab))/sum(cTab)*100

Variance <- abs(Accu.tra - Accu.val)

nTree; mTry; maxLeaf
# 문제는 랜덤포레스트 방법은 RF값이 나와도 그 이유를 설명 못한다는 것이다? 그래서 DT와 RF 방법은 비교불가다.

Accu.tra
Accu.val
Variance

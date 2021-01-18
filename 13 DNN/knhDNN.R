library(tensorflow)
library(keras)
install.packages('caTools')
install.packages('e1071')
install.packages('neuralnet')
library(caTools)
library(e1071)
library(neuralnet)
D.data <- read.csv('bCancerWC.csv', header = TRUE)

Tra<- read.csv('Tra1.csv', header = TRUE)
Val<- read.csv('Val1.csv', header = TRUE)
Tes<- read.csv('Tes1.csv', header = TRUE)

wSeedlist <- c(12345, 12346, 50000, 12347, 12348, 12349, 12350, 12351, 12352, 12353)
for (wSeed in wSeedlist){
  
  
  tf$random$set_seed(wSeed)
  cat('\n Seed', wSeed)
  
  train <- sample.split(D.data$Cancer, SplitRatio = 0.6)
  
  training <- D.data[train, ]
  remainder <- D.data[!train, ]
  
  validate <- sample.split(remainder$Cancer, SplitRatio = 0.5)
  
  validation <- remainder[validate, ]
  test <- remainder[!validate, ]
  
  Tra <- training
  Val <- validation
  Tes <- test
  
  
  
  Tra$Cancer <- ifelse(Tra$Cancer == 2, 0, 1)
  Val$Cancer <- ifelse(Val$Cancer == 2, 0, 1)
  Tes$Cancer <- ifelse(Tes$Cancer == 2, 0, 1)
  
  x_Tra <- as.matrix(subset(Tra, select = -c(Record, Cancer)))
  y_Tra <- as.matrix(Tra$Cancer)
  x_Val <- as.matrix(subset(Val, select = -c(Record, Cancer)))
  y_Val <- as.matrix(Val$Cancer)
  x_Tes <- as.matrix(subset(Tes, select = -c(Record, Cancer)))
  y_Tes <- as.matrix(Tes$Cancer)
  
  
  
  #layer 표현...매트리스 차원 몰라서 널. 은닉층 당 노드 개수.
  model <- keras_model_sequential() %>% layer_dense(units = 10, input_shape = c(9, NULL))%>% 
    layer_dense(units = 20, activation = 'relu') %>%
    layer_dense(units = 30, activation = 'relu') %>%
    layer_dense(units = 15, activation = 'relu') %>%
    layer_dense(units = 7, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  
  lRate <- 0.1
  model %>% compile(optimizer = optimizer_sgd(lr = lRate), loss = 'binary_crossentropy', metrics = c('accuracy')) #학습방법
  
  for(i in 1:5){
    
    nEpochs <- 100
    model %>% fit(x_Tra, y_Tra, epochs = nEpochs, verbose = 0) #verbose는 결과출력 양식 결정.
    
    
    #accTra <- model %>% evaluate(x_Tra, y_Tra, verbose = 0)
    #accVal <- model %>% evaluate(x_Val, y_Val, verbose = 0)
    
    #Accu.tra <- round(accTra$accuracy*100, digits = 2)
    #Accu.val <- round(accVal$accuracy*100, digits = 2)
    
    Upto <- nEpochs*i
    
    #cat(sprintf('\n Epoch : %5d  Train : %6.2f%%  Valid : %6.2f%%', Upto, Accu.tra, Accu.val))
    
    
    accTes <- model %>% evaluate(x_Tes, y_Tes, verbose = 0)
    Accu.tes <- round(accTes$accuracy*100, digits = 2)
    cat(sprintf('\n Epoch : %5d  Test : %6.2f%%', Upto, Accu.tes))
  }
  

}

###############밑에는 수업 때 한 거.####################################################
Tra$Cancer <- ifelse(Tra$Cancer == 2, 0, 1)
Val$Cancer <- ifelse(Val$Cancer == 2, 0, 1)
Tes$Cancer <- ifelse(Tes$Cancer == 2, 0, 1)

x_Tra <- as.matrix(subset(Tra, select = -c(Record, Cancer)))
y_Tra <- as.matrix(Tra$Cancer)
x_Val <- as.matrix(subset(Val, select = -c(Record, Cancer)))
y_Val <- as.matrix(Val$Cancer)
x_Tes <- as.matrix(subset(Tes, select = -c(Record, Cancer)))
y_Tes <- as.matrix(Tes$Cancer)

wSeed <- 13579
tf$random$set_seed(wSeed)

#layer 표현...매트리스 차원 몰라서 널. 은닉층 당 노드 개수.
model <- keras_model_sequential() %>% layer_dense(units = 20, input_shape = c(9, NULL))%>% 
  layer_dense(units = 30, activation = 'relu') %>%
  layer_dense(units = 15, activation = 'relu') %>%
  layer_dense(units = 7, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

lRate <- 0.1
model %>% compile(optimizer = optimizer_sgd(lr = lRate), loss = 'binary_crossentropy', metrics = c('accuracy')) #학습방법

for(i in 1:5){
  
nEpochs <- 100
model %>% fit(x_Tra, y_Tra, epochs = nEpochs, verbose = 0) #verbose는 결과출력 양식 결정.


accTra <- model %>% evaluate(x_Tra, y_Tra, verbose = 0)
accVal <- model %>% evaluate(x_Val, y_Val, verbose = 0)
#accTra
#accVal
#accTra$accuracy
Accu.tra <- round(accTra$accuracy*100, digits = 2)
Accu.val <- round(accVal$accuracy*100, digits = 2)

Upto <- nEpochs*i
cat(sprintf('\n Epoch : %5d  Train : %6.2f%%  Valid : %6.2f%%', Upto, Accu.tra, Accu.val))
}

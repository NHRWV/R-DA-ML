setwd("C:/R DA-ML/7 Assoc")
rm(list = ls()) # environment 클리어

install.packages("arules")
library(arules)

A.data <- read.transactions("POS2020.csv", sep = ',', format = "basket", skip = 1, cols = 1)

summary(A.data)

inspect(A.data)
inspect(A.data[1:5])

itemFrequencyPlot(A.data)
itemFrequencyPlot(A.data, support = 0.3)
itemFrequencyPlot(A.data, topN = 10)

apriori(A.data)
apriori(A.data, parameter = list(support = 0.2))
apriori(A.data, parameter = list(confidence = 0.9))
apriori(A.data, parameter = list(support = 0.2, confidence = 0.5)) #20개 나오는 파라미터.

rule20 <- apriori(A.data, parameter = list(support = 0.2, confidence = 0.5))
inspect(rule20)
inspect(sort(rule20, by = "lift")) # 리프트값에 따라 정렬.
inspect(sort(rule20, by = "lift")[1:5])
inspect(sort(rule20, decreasing = FALSE, by = "lift")[1:5])

sodaR <- subset(rule20, items %in% 'soda') #soda가 있는 규칙을 찾아라.
inspect(sodaR)

sodaRc9 <- subset(rule20, items %in% 'soda' & confidence > 0.9)
inspect(sodaRc9)

scR <- subset(rule20, items %in% c('soda', 'cracker'))
inspect(scR)

scaR <- subset(rule20, items %ain% c('soda', 'cracker')) # 소다와 크래커 둘 다 있는 경우 찾기.
inspect(scaR)

sbaR <- subset(rule20, items %ain% c('soda', 'beer'))
inspect(sbaR)

thenbeerR <- subset(rule20, rhs %in% 'beer') #맥주가 잘 팔리는 조합 구하기.
inspect(thenbeerR)

whiskeyR <- subset(rule20, items %in% 'whiskey')
inspect(whiskeyR)

erR <- subset(rule20, items %pin% 'er') # 'er'이 부분적으로 들어간 아이템에 관해 찾아라
inspect(erR)

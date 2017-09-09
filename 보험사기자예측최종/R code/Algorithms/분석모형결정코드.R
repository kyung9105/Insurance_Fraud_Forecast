# 훈령용 데이터와 테스트용 데이터의 추출

install.packages("dplyr")
library(dplyr)

New_algorithm2 = read.csv("11번째알고리즘(고객ID).csv")

training = filter(New_algorithm2, New_algorithm2$데이터셋구분 ==1)
testing = filter(New_algorithm2, New_algorithm2$데이터셋구분 ==2)

training2 = training[,c(-1,-3)]
testing2 = testing[,c(-1,-3)]


# 의사결정트리 

install.packages("party")
library(party)
install.packages("caret")
library(caret)
install.packages("glm")
library(glm)
install.packages("nnet")
library(nnet)
install.packages("data.table")
library(data.table)

training2= data.table(training2)

set.seed(1)
ctree_fit = train(보험사기자여부~. , data=training2, method="ctree")
# 의사결정트리
set.seed(2)
rf_fit = train(보험사기자여부~. , data=training2, method="rf")
# 랜덤포레스트
set.seed(3)
glm_fit = train(보험사기자여부~. , data=training2, method="glm")
# 회귀
set.seed(4)
nnet_fit = train(보험사기자여부~. , data=training2, method="nnet")
#인공신경망 

prediction = predict(nnet_fit,testing2)

Precision = confusionMatrix(prediction, testing2$보험사기자여부)$byClass[4]
Recall = confusionMatrix(prediction, testing2$보험사기자여부)$byClass[2]
F_measure = 2*(Precision*Recall)/(Precision+Recall)
F_measure

# F_measure의 탐색

install.packages("caret")
library(caret)
install.packages("data.table")
library(data.table)

training2 = data.table(training2)

set.seed(1000)
rf_fit1 = randomForest(보험사기자여부~. , data=training2, mtry=23, ntree=500 ,importance=TRUE)
# randomForest 패키지 활용 ( parameter 지정 )

set.seed(999)
rf_fit2 = train(보험사기자여부~. , data=training2, method="rf")
# caret 패키지의 랜덤포레스트 알고리즘

set.seed(998)
parRF_fit = train(보험사기자여부~. ,data=training2, method="parRF")
#parRF 모델

set.seed(997)
extraTrees_fit = train(보험사기자여부~. ,data=training2, method="extraTrees")
# extraTrees 모델

prediction = predict(rf_fit,testing2)

Precision = confusionMatrix(prediction, testing2$보험사기자여부)$byClass[4]
Recall = confusionMatrix(prediction, testing2$보험사기자여부)$byClass[2]
F_measure = 2*(Precision*Recall)/(Precision+Recall)
F_measure

# F_measure 탐색
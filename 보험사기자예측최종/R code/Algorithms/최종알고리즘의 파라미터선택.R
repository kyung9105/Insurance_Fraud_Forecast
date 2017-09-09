set.seed(1004)
rfControl=trainControl(method="cv", number=10)

set.seed(998)
rf_fit = train(보험사기자여부~. , data=training2, trControl=rfControl )

plot(rf_fit)
# caret 패키지에서 지정해주는 parameter

rfGrid = expand.grid(mtry=1:23)
rf_fit2 = train(보험사기자여부~. , data=training2, trControl=rfControl, tuneGrid = rfGrid)

trellis.par.set(caretTheme())
plot(rf_fit2)
# 첫번째 tune parameter

rfControl2 = trainControl(method="cv", number =10, classProbs = TRUE, search="random")
rf_fit3 = train(보험사기자여부~. , data=training2, method="rf", tuneLength=30, trControl=rfControl2)

trellis.par.set(caretTheme())
plot(rf_fit3)
# 두번째 tune parameter


plot(varImp(rf_fit), top=10)
# 변수중요도 
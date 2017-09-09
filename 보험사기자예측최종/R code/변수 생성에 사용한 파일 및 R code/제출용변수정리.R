install.packages("dplyr")
library(dplyr)
setwd("C:/r_temp")

Customer_info = read.csv("Customer Information Data.csv", header=TRUE)
Customer_Contract = read.csv("Customer Contract Data.csv", header=TRUE)
Customer_Claim = read.csv("Customer's Claim Data.csv", header=TRUE)

Customer_info = Customer_info[order(Customer_info$고객ID),]

problem_df = merge(Customer_info, Customer_Contract, by="고객ID")
problem_df2 = merge(problem_df, Customer_Claim, by=c("고객ID","증권번호"))


#1 동일병명으로 중복신청한 개수
pattern1 = read.csv("Claim Data(pattern1).csv")

pattern1 = mutate(pattern1, 동일병명으로중복신청한개수 = pattern1$고객의.총.청구.횟수-pattern1$코드.중복을.제거한.실제.신청수)


Customer_info$동일병명으로중복신청한개수 <- pattern1$동일병명으로중복신청한개수


#2 계약체결년월 개수
a1 = with(problem_df,tapply(계약체결년월, 고객ID,function(x) length(unique(x))))

Customer_info$계약체결년월개수 <- a1

#3 한달최대 계약개수
problem_df3=problem_df[order(problem_df$고객ID,problem_df$계약체결년월),]

for(i in 1:113010){
  ifelse(problem_df3$계약체결년월[i] == problem_df3$계약체결년월[i+1], problem_df3$b1[i] <- 1, problem_df3$b1[i] <- 0)
}
problem_df3$b2[1] <- 0
for(i in 2:113010){
  ifelse(problem_df3$b1[i] == 1 , problem_df3$b2[i]<- problem_df3$b2[i-1]+1,  problem_df3$b2[i]<- 0 )
}

a2 = with(problem_df3,tapply(b2, 고객ID, max))

Customer_info$하루최대계약개수 <- a2+1

#4 지불승은된 증권개수

a3 = with(problem_df2, tapply(증권번호, 고객ID, function(x) length(unique(x))))

Customer_info$지불승인된증권개수 <- a3

#5 지불신청한증권개수

a4 = with(problem_df2, tapply(증권번호, 고객ID, function(x) length(x)))
Customer_info$지불신청한증권개수 <- a4

#6 고객이체결한증권개수

a5 = with(problem_df, tapply(증권번호, 고객ID, function(x) length(unique(x))))
Customer_info$고객이체결한증권개수 <- a5

#7 보장성보험청구횟수

New_df2=problem_df2[!duplicated(problem_df2[c("고객ID","증권번호")]),]

for(i in 1:119020){
  ifelse(problem_df2$상품분류[i] == "변액CI" | problem_df2$상품분류[i] =="변액종신"| problem_df2$상품분류[i] =="보장" |problem_df2$상품분류[i] =="실손"| problem_df2$상품분류[i] =="암" |problem_df2$상품분류[i] =="어린이"| problem_df2$상품분류[i] =="일반CI"
         |problem_df2$상품분류[i] =="일반종신" | problem_df2$상품분류[i] =="정기", problem_df2$V1[i] <- 1, problem_df2$V1[i] <- 0)
}

a6 = with(problem_df2, tapply(V1, 고객ID, function(x) sum(x)))
Customer_info$보장성보험청구횟수 <- a6


#8 가입한계약의 종류

a7 = with(problem_df, tapply(상품분류, 고객ID, function(x) length(unique(x))))
Customer_info$가입한계약의종류 <- a7

#9 유의병원방문총횟수

for(i in 1:119020){
  ifelse(problem_df2$유의병원여부[i] == 0, problem_df2$V2[i] <- 0, problem_df2$V2[i] <- 1 )
}

a8 = with(problem_df2, tapply(V2, 고객ID, function(x) sum(x)))
Customer_info$유의병원방문총횟수 <- a8

#10 고객이 신청한 질병 사유의개수

a9 = with(problem_df2, tapply(원인코드, 고객ID, function(x) length(unique(x))))
Customer_info$고객이신청한질병사유의개수 <- a9

#11 고객이 만난 의사의 명수

a10 = with(problem_df2, tapply(담당의사면허번호, 고객ID, function(x) length(unique(x))))
Customer_info$고객이만난의사의명수 <- a10

#12 고객이 방문한 병원의 개수

a11 = with(problem_df2, tapply(병원코드, 고객ID, function(x) length(unique(x))))
Customer_info$고객이방문한병원의개수 <- a11

#13 유효입통원 총일수

a12 = with(problem_df2, tapply(유효입원.통원일수, 고객ID, function(x) sum(x)))
Customer_info$유효입통원총일수 <- a12

#14 진료과목개수

problem_df2$진료과목개수[is.na(problem_df2$진료과목개수)] <- 1

a14 = with(problem_df2, tapply(진료과목개수, 고객ID, function(x) sum(x)))
Customer_info$진료과목개수 <- a14

#15 FP변경횟수

for(i in 1:119020){
  ifelse(problem_df2$FP.변경.여부[i] =="Y", problem_df2$V5[i]<-1, problem_df2$V5[i]<-0) 
}

a18 = with(problem_df2, tapply(V3, 고객ID, function(x) sum(x)))

Customer_info$FP변경횟수 <- a188

#16 실손처리의개수

for(i in 1:119020){
  ifelse(problem_df2$실손처리여부[i]=="Y", problem_df2$V8[i] <-1 , problem_df2$V8[i] <-0)
}

a23 = with(problem_df2, tapply(V8, 고객ID, function(x) sum(x)))

Customer_info$실손처리개수 <- a23

#17 가족군별신청횟수

a24 = read.csv("가족군별 신청횟수.csv")

Customer_info$가족군별신청횟수 <- a24$가족.청구.총합

#18 고액보험가입비중

problem_df2$주보험금[is.na(problem_df2$주보험금)]<- 0
a16 = with(problem_df2, tapply(주보험금, 상품분류, function(x) mean(x)))

problem_df2$V3 <- 0
problem_df2$V3[which(problem_df2$상품분류=="교육")] <- 12808712
problem_df2$V3[which(problem_df2$상품분류=="변액CI")] <- 45357318
problem_df2$V3[which(problem_df2$상품분류=="변액연금")] <- 23623956
problem_df2$V3[which(problem_df2$상품분류=="변액저축")] <- 18421348
problem_df2$V3[which(problem_df2$상품분류=="변액종신")] <- 51780697
problem_df2$V3[which(problem_df2$상품분류=="보장")] <- 16235525
problem_df2$V3[which(problem_df2$상품분류=="실손")] <- 50000000
problem_df2$V3[which(problem_df2$상품분류=="암")] <- 17825565
problem_df2$V3[which(problem_df2$상품분류=="어린이")] <- 21727398
problem_df2$V3[which(problem_df2$상품분류=="어린이연금")] <- 8220000
problem_df2$V3[which(problem_df2$상품분류=="어린이연금_변액")] <- 12000000
problem_df2$V3[which(problem_df2$상품분류=="어린이저축")] <- 9532620
problem_df2$V3[which(problem_df2$상품분류=="어린이저축_변액")] <- 16258427
problem_df2$V3[which(problem_df2$상품분류=="일반CI")] <- 41155834
problem_df2$V3[which(problem_df2$상품분류=="일반연금")] <- 25121973
problem_df2$V3[which(problem_df2$상품분류=="일반저축")] <- 17013065
problem_df2$V3[which(problem_df2$상품분류=="일반종신")] <- 51155214
problem_df2$V3[which(problem_df2$상품분류=="정기")] <- 39834051

problem_df2 = mutate(problem_df2,고액보험=problem_df2$주보험금-problem_df2$V3)

for(i in 1: 119020){
  ifelse(problem_df2$고액보험[i] > 0, problem_df2$V4[i] <- 1, problem_df2$V4[i] <- 0)
}

a17 = with(problem_df2, tapply(V4, 고객ID, function(x) sum(x)))

Customer_info$고액보험 <- a17

#19 계약후 바로 사고발생의 경우

a19 = read.csv("기간.csv")
problem_df2$기간 <- a19$기간.일.

a20 = filter(problem_df2, problem_df2$보험사기자여부=="N") 
a21 = round(mean(problem_df2$기간))

problem_df2$V6 <- a21

problem_df2 = mutate(problem_df2, 계약과지불기간= problem_df2$기간-problem_df2$V6)

for(i in 1: 119020){
  ifelse(problem_df2$계약과지불기간[i] > 0, problem_df2$V7[i] <- 1, problem_df2$V7[i] <- 0)
}

a22 = with(problem_df2, tapply(V7, 고객ID, function(x) sum(x)))

Customer_info$계약과지불기간 <- a22

#20 실제사건보다 추가청구한 횟수

pattern1 = mutate(pattern1, 실제사건보다추가적으로보험청구한개수 = pattern1$총.발생한.사고.수*pattern1$고객이.가진.증권의.개수-pattern1$고객의.총.청구.횟수)

for(i in 1:22400){
  ifelse(pattern1$실제사건보다추가적으로보험청구한개수[i] >= 0, pattern1$A1[i] <- 0, pattern1$A1[i] <- pattern1$실제사건보다추가적으로보험청구한개수[i]*-1)
}

Customer_info$실제사건보다추가청구 <- pattern1$A1

#21 사기FP계약개수

a25 = read.csv("고객ID 변수 통합.csv")

Customer_info$사기FP계약개수 <- a25$사기FP포함.여부

#22 신용등급변화량

Customer_info$신용등급.최소.[is.na(Customer_info$신용등급.최소.)] <- 6
Customer_info$신용등급.최대.[is.na(Customer_info$신용등급.최대.)] <- 6
Customer_info$신용등급.최소.[which(Customer_info$신용등급.최소.==99)] <- 6
Customer_info$신용등급.최대.[which(Customer_info$신용등급.최대.==99)] <- 6

Customer_info= mutate(Customer_info, 신용등급변화량=Customer_info$신용등급.최대.- Customer_info$신용등급.최소.)

Algorithm_data$신용등급변화량 <- Customer_info$신용등급변화량


#23 돈관련변수 : 고객소득수준

Customer_info = mutate(Customer_info, 돈관련변수 = Customer_info$고객추정소득+Customer_info$추정가구소득1+Customer_info$추정가구소득2)
Customer_info[is.na(Customer_info$돈관련변수),] <- 0

Customer_info$고객소득수준 <- Customer_info$돈관련변수



#최종 filtering
Algorithm_data=Customer_info[,c(1,3,2,26:49)]

write.csv(Algorithm_data, "알고리즘데이터(전체고객ID).csv")

setwd("C:\\Users\\user\\Desktop\\2021-1\\데이터 마이닝 입문\\실습\\Decisition Tree")

#대출 가능여부 파악>>화이트박스 알고리즘을 사용해야함

#stringsAsFactors 디폴트값이 False

credit=read.csv("credit.csv", stringsAsFactors = TRUE)
head(credit)


summary(credit)
table(credit$checking_balance) #빈도수를 제대로 보려고
table(credit$savings_balance)
summary(credit$savings_balance) #카테고리형 변수면 summary=table 

summary(credit$months_loan_duration)
summary(credit$amount)

#y는 default 임
summary(credit$default)


#데이터의 행순서를 한번 섞어서 데이터 쪼개기
set.seed(12345)
credit_rand=credit[order(runif(1000)),]
credit_rand

#확인
head(credit$amount)
head(credit_rand$amount)


#데이터 쪼개기
credit_train=credit_rand[1:900,]
credit_test=credit_rand[911:1000,]
table(credit_train$default)
table(credit_test$default)

#DT 적용 - C5.0 모델 활용
install.packages("C50")
library("C50")

#모델 생성
m=C5.0(credit_train[,-17],credit_train$default)
m
summary(m)

#모델 테스트
p=predict(m,credit_test) #-17안해도 되는 이유는 모델이 알아서 변수 찾아서 하기 때문
p
library(gmodels)
CrossTable(credit_test$default, p)


#부스팅 >> 모델의 성능을 개선시키는 >> 가중치
credit_boosting=C5.0(credit_train[,-17], credit_train$default,trials=10)
credit_boosting
summary(credit_boosting)

boost_predict=predict(credit_boosting, credit_test)
boost_predict

CrossTable(credit_test$default, boost_predict)

#은행 입장에서 대출 가능한데 불가능해! 보다는
#불가능한데 가능해!라고 하는게 critical함
#이런 것들을 반영해서 할 수 있음
 
error_cost=matrix(c(0,1,4,0), nrow=2)
error_cost

credit_cost_m=C5.0(credit_train[-17], credit_train$default, costs = error_cost)
summary(credit_cost_m)

cost_predit= predict(credit_cost_m, credit_test)

CrossTable(credit_test$default, cost_predit)




#tree 패키지 활용하기
install.packages("tree")
library(tree)
credit_tree=tree(credit_train$default~.,data=credit_train)
summary(credit_tree)
#그림 그리기
plot(credit_tree)
text(credit_tree)

#prunning 
cv.trees= cv.tree(credit_tree, FUN=prune.misclass)
plot(cv.trees) #5일 때 missclass 가자 ㅇ적음

credit_tree_prun=prune.misclass(credit_tree, best=5)
plot(credit_tree_prun)
text(credit_tree_prun)

#예측 
credit_tree_pred= predict(credit_tree_prun, credit_test, type="class") #타입 설정해줘야됨
CrossTable( credit_tree_pred,credit_test$default)





setwd("C:\\Users\\user\\Desktop\\2021-1\\데이터 마이닝 입문\\실습\\regression")
insurace=read.csv("insurance.csv",stringsAsFactors = TRUE) #디폴트가 false
summary(insurace)
str(insurace)
insurace
hist(insurace$charges)

table(insurace$smoker)
table(insurace$sex)


cor(insurace)#계산해야되므로 반드시 수치형이어야함
cor(insurace[c("age","bmi","children","charges")])

cor.test(insurace$"age",insurace$charges)
#pvalue 굉장히 작으므로 H0 reject


#산포도 매트릭스 그리기
pairs(insurace[c("age","bmi","children","charges")])
#install.packages("psych")
library(psych)

pairs.panels(insurace[c("age","bmi","children","charges")])




#모델 만들기
ins_model=lm(charges~age+children+bmi+sex+smoker+region, data=insurace)

ins_model #변수 형태가 넣은 거랑 좀 다름
#명목형 변수가 들어가면 r이 내부적으로 더미코딩을 자동으로 해줌

#변수 다 쓰기 귀찮을 때
ins_model=lm(charges~.,data=insurace)
ins_model


###############
##검정 스타투##
###############

summary(ins_model)
#t값= estimate/std.error 
#p가 8개(더미코딩때매)



#모델 성능 향상-비선형 관계 추가
str(insurace)
insurace$age2=insurace$age^2

#수치변수를 이진 지시자로 변환
insurace$bmi
insurace$bmi30= ifelse(insurace$bmi>=30,1,0)

str(insurace)


ins_model2=lm(charges~age+age2+children+bmi+bmi30+smoker+sex+region,data=insurace)

summary(ins_model2)


#############
##predict - 임의 데이터 가지고
###########
insurace$pred=predict(ins_model2,insurace)

cor(insurace$pred,insurace$charges)

#pred 보다는 모델의 유의성을 연구 위주

plot( insurace$pred,insurace$charges)

abline(a=0,b=1,col="red")




predict(ins_model2,data.frame(age=0,age2=30^2,children=2,bmi=30,bmi30=1,
                              sex="male", smoker="no",region="northeast"))


#모형의 평가지표 구하기
install.packages("forecast")
library(forecast)

accuracy(ins_model2)

#다중공선성 진단 보통4이상이면 다중공선성있다고 판단.
install.packages("car")

library(car)
vif(ins_model)


















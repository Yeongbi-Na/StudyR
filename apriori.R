setwd("C:\\Users\\user\\Desktop\\2021-1\\데이터 마이닝 입문\\실습\\apriori")
#install.packages("arules")
library(arules)
grocerie=read.transactions("groceries.csv",sep=",")
grocerie
summary(grocerie) 

inspect(grocerie[1:5])

#head(inspect(grocerie)) 하면 다 나옴

itemFrequency(grocerie[,1:3]) #1~3까지 아이템에 대한 support르 보겠다다
#내부적으로 알파벳 순서로 정렬되어있음

itemFrequencyPlot(grocerie[,1:10])
itemFrequencyPlot(grocerie, support=0.1) #0.1보다 큰 아이템만
itemFrequencyPlot(grocerie, topN=20) #상위 20개


image(grocerie[1:5]) #1~5행을 도식화
image(grocerie[1:50]) #1~50행을 도식화
image(grocerie[1:100]) #1~100행을 도식화
#날짜 순이라면 검은 점의 패턴이 계절적 영향을 줄 수 있음




############
##모델 훈련#
############
#무언가를 예측하는 모델이었는데 이건 unsupervised learning 이기 때문에
#모델을 만드는 것이 아닌 데이터간 관계를 알아냄


apriori(data=grocerie, parameter=list(support=0.1, confidence=0.8))
#set of 0 rules  으로 나옴

apriori(data=grocerie, parameter=list(support=0.05, confidence=0.5))
#아이템이 169개이면 많은 게 아님. 그래서 rule을 도출하기가 어려움

grocerirules=apriori(data=grocerie, parameter=list(support=0.006, confidence=0.25,minlen=2))
#463개

summary(grocerirules)
#lift의 의미: 그냥 y를 구매하는 사람에 비해 x>y 구매하는 사람이 얼마나 많은지
#lift 값이 1이상 되어야 의미 있다 볼 수 있음


inspect(grocerirules[1:3])

inspect(sort(grocerirules,by="lift")[1:5])


berryrules=subset(grocerirules, items %in% "berries")
inspect(berryrules)

berryyogurtrules=subset(grocerirules, items %in% c("berries","yogurt"))
inspect(berryyogurtrules)


fruitrules=subset(grocerirules,items %in% "fruit")#암것도 안나옴 tropical fruit 이런 식이라
fruitrules=subset(grocerirules,items %pin% "fruit") #partial in으로 하면 나옴
inspect(fruitrules)

#베리, 요거트 동시 포함
berryyogurtrules2=subset(grocerirules, items %ain% c("berries","yogurt"))#all in
inspect(berryyogurtrules2)

berryyogurtrules3=subset(grocerirules, items %in% c("berries","yogurt"),confidence=0.5)
inspect(berryyogurtrules3)

herbrules=subset(grocerirules,lhs %in% "herbs") #왼쪽의 아이템 lhs 
inspect(herbrules)


#파일로 저장하기
#write(grocerirules, file="groceryrules.csv",sep=",",row.names=FALSE)


#데이터프레임으로 변환
grocerirules_df=as(grocerirules,"data.frame")
grocerirules_df



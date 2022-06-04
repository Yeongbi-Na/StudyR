setwd("C:\\Users\\user\\Desktop\\2021-1\\데이터 마이닝 입문\\실습\\clustering")


teens=read.csv("snsdata.csv",stringsAsFactors = TRUE)
head(teens)
str(teens)

#결측값?!
table(teens$gender) #두개 합쳐서 30000개 되어야하는데 그렇지 않음

table(teens$gender, useNA="ifany")

#numeric의 결측치 확인
summary(teens$age) #나이가 3살인 사람이 sns를?, max가 106?

#teenager만 남기기
teens$age=ifelse(teens$age>=13&teens$age<20,teens$age,NA)
summary(teens$age)


#clustering은 거리기반이기 때문에 결측치 처리를 해주어야함
#결측치 있는 데이터를 배제하는 것보다는 guessing해서 처리해주면 제일 좋음
#나이와 성별의 결측치를 넣어주어야함
teens$female=ifelse(teens$gender=="F" & !is.na(teens$gender), 1,0)
teens$no_gender=ifelse(is.na(teens$gender),1,0)
table(teens$female,useNA="ifany")
table(teens$no_gender,useNA="ifany")

mean(teens$age, na.rm=TRUE)

str(teens) #졸업년도 활용
aggregate(data=teens, age~gradyear, mean,na.rm=TRUE)
ave_age=ave(teens$age, teens$gradyear,FUN=function(x) mean(x,na.rm=TRUE)) 
#gradyear별 age의 평균값들 중 하나의 row에 해당하는 gradyear에 대한 age평균값을 을 반환
ave_age

teens$age=ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)


##################################
#k-means clustering#

str(teens)

interest=teens[5:40] #5~40열만 사용
str(interest)

#거리기반이므로 z score정규화
a=as.data.frame(scale(interest))
str(a)
interest_z
interest_z=as.data.frame(lapply(interest, scale)) #lapply 하면 list로 반환해줌
str(interest_z)

myclusters=kmeans(interest_z,5)
myclusters$size
myclusters$centers #각 열에 대한 cluster의 중심값을 반환
#여기서부터는 해석
teens$cluster=myclusters$cluster
str(teens)

teens[1:5,] #gender에 직접 결측치 처리 하지 않았기 때문에 gender에 NA 남아있음
aggregate(data=teens, age~cluster, mean)
aggregate(data=teens, female~cluster, mean)






setwd("C:\\Users\\user\\Desktop\\2021-1\\데이터 마이닝 입문\\실습\\knn")
data=read.csv("wisc_bc_data.csv")
#summary(data)
str(data)

#필요없는 데이터 id 없애기
wbcd=data[-1]

#diagnosis명목형 변수를 factor형으로 바꾸기 - wbcd$diagnosis=factor(wbcd$diagnosis) 만 해 ok 그냥 설명할려고 길게
wbcd$diagnosis=factor(wbcd$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
table(wbcd$diagnosis)
prop.table(table(wbcd$diagnosis))
round(prop.table(table(wbcd$diagnosis)),2)


#다른 변수들 살펴보기
summary(wbcd[c("radius_mean","area_mean","dimension_worst")])
summary(wbcd) #변수들 사이 스케일 차이가 큼> knn은 스케일에 예민함


#정규화
normalize =function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

normalize(c(1,2,3,4,5)) #예시
wbcd_n=lapply(wbcd[2:31],normalize)
wbcd_n #리스트형태로 바꿔짐, df형태로 바꿔줘야됨

wbcd_n=as.data.frame(wbcd_n)
wbcd_n
summary(wbcd_n)

#####################
###드디어 knn 적용###
#####################
#1~469는 train/ 470~569번째는 test용

wbcd_train=wbcd_n[1:469,]
wbcd_test=wbcd_n[470:569,] #순서가 random하지 않다면 random하게 잘라줘야됨

#y값을 추출
wbcd_train_labels=wbcd[1:469,1]
wbcd_test_labels=wbcd[470:569,1]
wbcd_test_labels

#knn(train,test,class,k)< 예측된 y값을 반환함
install.packages("class")
library(class)
wbcd_test_pred=knn(wbcd_train,wbcd_test,wbcd_train_labels,k=21)
wbcd_test_pred

#confusion matrix
install.packages("gmodels")
library(gmodels)
CrossTable(wbcd_test_labels, wbcd_test_pred)
#암인데 암이 아니라고 예측한 것은 매우 치명적>FN이 FP보다 치명적
#해당 분야에서는 극단적인 이상치가 존재할 수가 있기 때문에 
#이를 보다 잘 감지하면 성능 높아질 수도...? 예상
#z점수 표준화가 최소최대가 정해져 있지 않기 때문에 이를 해보쟈

#정규화2- Z점수 표준화- scale()
str(wbcd)
wbcd_z=scale(wbcd[-1])
wbcd_z=as.data.frame(wbcd_z)
wbcd_z
str(wbcd_z)
summary(wbcd_z) #평균이 모두 0

#다시 knn 적용
wbcd_train=wbcd_z[1:469,]
wbcd_test=wbcd_z[470:569,]
wbcd_train_labels=wbcd[1:469,1]
wbcd_test_labels=wbcd[470:569,1]
wbcd_test_pred2=knn(wbcd_train,wbcd_test,wbcd_train_labels,k=21)
wbcd_test_pred2

#Confusion Matrix-Z 표준화
CrossTable(wbcd_test_pred2,wbcd_test_labels)
#성능이 좋아지지 않음
#but 모델의 성능을 높이는 방법을 찾고 모델을 발전시키는
#연습 꼭!! 


####knn 성능 높이기### k 값 바꾸면서
wbcd_test_pred3=knn(wbcd_train,wbcd_test,wbcd_train_labels,k=23)
CrossTable(wbcd_test_pred2,wbcd_test_labels)

wbcd_test_pred3=knn(wbcd_train,wbcd_test,wbcd_train_labels,k=15)
CrossTable(wbcd_test_pred2,wbcd_test_labels)

#recall, accuracy, F1 등 분야에 따라 중요도에 따라
#초점을 맞춰서 성능을 높임








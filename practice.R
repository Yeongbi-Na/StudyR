setwd("C:\\Users\\user\\Desktop\\2021-1\\데이터 마이닝 입문\\실습\\Naive Bayes")
sms_raw=read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)

sms_raw$type=factor(sms_raw$type)
sms_raw$type
str(sms_raw)
table(sms_raw)
prop.table(table(sms_raw$type))

library(tm)
sms_corpus=Corpus(VectorSource(sms_raw$text))
sms_corpus
inspect(sms_corpus[1:10])


#전처리
corpus_clean=tm_map(sms_corpus,content_transformer(tolower)) #소문자로
corpus_clean=tm_map(corpus_clean,removeNumbers)
corpus_clean=tm_map(corpus_clean,removeWords,stopwords())
corpus_clean=tm_map(corpus_clean,removePunctuation)

inspect(corpus_clean[1:10])

#매트릭스 형태로
sms_dtm=DocumentTermMatrix(corpus_clean)
inspect(sms_dtm)

#train test data 나누기
sms_raw_train=sms_raw[1:4169,]
sms_raw_test=sms_raw[4170:5559, ]

sms_dtm_train=sms_dtm[1:4169,]
sms_dtm_test=sms_dtm[4170:5559,]

sms_corpus_train=corpus_clean[1:4169]
sms_corpus_test=corpus_clean[4170:5559]

#나눈 데이터 확인
table(sms_raw_train$type)
prop.table(table(sms_raw_train$type))


#5번 이상 출현한 단어만 >>findFreqTerms
sms_dict=findFreqTerms(sms_dtm_train ,5)


sms_train=DocumentTermMatrix(sms_corpus_train, list(dictionary=sms_dict))
sms_test=DocumentTermMatrix(sms_corpus_test, list(dictionary=sms_dict))

#빈도 단어에 대한 지표 생성 > 빈도가 아닌 출현 여부 생성
convert_count=function(x){
  x=ifelse(x>0,1,0)
  x=factor(x,levels=c(0,1),labels=c("No","Yes"))
  return (x)
  }

convert_count(1)

sms_train=apply(sms_train,MARGIN = 2, convert_count)
sms_test=apply(sms_test, MARGIN=2,convert_count)


#나이브 베이즈 적용
install.packages("e1071")
library(e1071)

#train 데이터로 모델 만들기
sms_classifier=naiveBayes(sms_train,sms_raw_train$type)
sms_classifier # 사전 확률 볼 수 있음(관찰된 확률)

#predict
sms_test_pred=predict(sms_classifier,sms_test)
sms_test_pred[1:10]


#confusion matrix
library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type)


#라플라스 추정기 설정해서 다시 모델
sms_classifier2=naiveBayes(sms_train,sms_raw_train$type, laplace=1)
sms_test_pred2=predict(sms_classifier2,sms_test)

CrossTable(sms_test_pred2, sms_raw_test$type)
##############################################
#실제 햄인데 스팸으로의 예측이 많아짐>> 경우에 따라서 중요도가 달라질 수 있어서
# 상황에 따락 모델 1과 2를 선택적으로 사용
###############################################






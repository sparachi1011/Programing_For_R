setwd('D:/Koushik/AP/ML_AP')
Uni_Data<-read.csv('UniversalBank.csv')

head(Uni_Data)
View(Uni_Data)


?colsToFactors

colsToFactors = c("Family","Personal.Loan","Education",
                  "Securities.Account","CD.Account","Online","CreditCard")
for(i in colsToFactors){
  Uni_Data[,i] = as.factor(Uni_Data[,i])
}

str(Uni_Data)
summary(Uni_Data)

data1=Uni_Data[Uni_Data$Experience>0,]
summary(data1)

cor(data1[,c("Age","Experience","Income","Mortgage","CCAvg")])

data1$Age=NULL


##### train test split

set.seed(1234)
rows=1:nrow(data1)
trainRows=sample(rows,round(0.7*nrow(data1)))
View(trainRows)
str(trainRows)

trainData=data1[trainRows,]
testData=data1[-trainRows,]

summary(data1)


### Model building::

model=glm(Personal.Loan~.-Experience,###-ID-ZIP.Code,
          data=trainData,
          family=binomial(link = "logit"))


summary(model)


## Predictions
preds = predict(model,testData, type='response')
testData$preds = preds



testData$preds2 = ifelse(testData$preds > 0.4,1,0)
#View(testData)


table(testData$Personal.Loan,testData$preds2,dnn=c('actuals','preds'))


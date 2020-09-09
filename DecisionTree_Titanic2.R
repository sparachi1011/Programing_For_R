data1 = read.csv("C:/Users/phsivale/Documents/Trainings/titanic.csv",
                 na.strings=c(""," ","NA","?","  "))
summary(data1)

names(data1)
colsToUse = c('pclass','survived','sex','age','fare','sibsp','parch','embarked')
data1 = data1[,colsToUse]

data1$age[is.na(data1$age)] = median(data1$age,na.rm = T)
# data1$age[is.na(data1$age)] = median(data1$age,na.rm = T)

data1 = na.omit(data1)
summary(data1)

###
data1$pclass = as.factor(data1$pclass)
data1$survived = as.factor(data1$survived)

##### train test split
set.seed(123)
rows = 1:nrow(data1)
trainRows = sample(rows,round(0.7*nrow(data1)))
trainData = data1[trainRows,]
testData = data1[-trainRows,]

## decsionTree
library(rpart)
dtree1 = rpart(survived ~.,data=trainData,
               control = c(minsplit =1,maxdepth=10,cp=0))## Tuning with other hyper-params
               # control = c(cp=0.0082)) ## Tuning with CP
## Identifying the best CP Value
plotcp(dtree1)

### Viewing the tree
dtree1
summary(dtree1)
### Plotting Decision Tree
plot(dtree1,main="Classification Tree for Survived Class",
     margin=0.1,uniform=TRUE)
text(dtree1,use.n=T)
## This one looks slightly better
library(rpart.plot)
rpart.plot(dtree1)

#### drawing predictions from the test data
preds = predict(dtree1,testData)
preds = as.data.frame(preds)
# View(preds)
preds$preds_Class = ifelse(preds$`1` > 0.5,1,0)
## Checking for Prec Recall and other acc metrics
table(testData$survived,preds$preds_Class,dnn=c('actuals','preds'))

## Checking for the best possible CP value
printcp(dtree1)
plotcp(dtree1)

library(C50)
dtree2 = C5.0(survived ~.,data=trainData)
plot(dtree2)
preds2 = predict(dtree2,testData,type='prob')
table(testData$survived,preds2,dnn=c('actuals','preds'))

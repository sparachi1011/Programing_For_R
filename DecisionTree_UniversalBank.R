setwd('D:/Koushik/AP/ML_AP')
Uni_DTree<-read.csv('UniversalBank.csv')
head(Uni_DTree)
str(Uni_DTree)
summary(Uni_DTree)

set.seed(123)
rows= 1:nrow(Uni_DTree)
Train_Rows_DTree= sample(rows,round(0.8*nrow(Uni_DTree)))

Test_Rows_DTree=rows[-Train_Rows_DTree]

Train_Data_DTree_Uni_Bank=Uni_DTree[Train_Rows_DTree,]

Test_Data_Dtree_Uni_Bank=Uni_DTree[Test_Rows_DTree,]

head(Train_Rows_DTree)


library(rpart)
library(rpart.plot)



#####Decision Tree::::: Using RPART#####


DTree_Uni_Bank_Model= rpart(CreditCard~.,data=Train_Data_DTree_Uni_Bank,
                          ##control = c(cp=0.5))
                            control = c(minsplit =1,maxdepth=5,cp=0.004))

plotcp(DTree_Uni_Bank_Model)

DTree_Uni_Bank_Model

plot(DTree_Uni_Bank_Model,main="Classification Tree for Universal Bank",
     margin=0.1,uniform=TRUE)
text(DTree_Uni_Bank_Model,use.n=T)

rpart.plot(DTree_Uni_Bank_Model)


#### drawing predictions from the test data
preds = predict(DTree_Uni_Bank_Model,Test_Data_Dtree_Uni_Bank)
preds = as.data.frame(preds)
# View(preds)
##unique(preds)
##preds$preds_CreditCard = ifelse(preds$`1` > 0.5,1,0)

preds$preds_CreditCard = ifelse(preds > 0.5,1,0)
## Checking for Prec Recall and other acc metrics
table(Test_Data_Dtree_Uni_Bank$CreditCard,preds$preds_CreditCard,dnn=c('actuals','preds'))

printcp(DTree_Uni_Bank_Model)
plotcp(DTree_Uni_Bank_Model)



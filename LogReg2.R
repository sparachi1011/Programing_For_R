data1 = read.csv('C:/Users/phsivale/Documents/Trainings/universalBank.csv')
str(data1)

data1$ID = NULL
data1$ZIP.Code = NULL

colsToFactors = c("Family","Personal.Loan","Education",
                  "Securities.Account","CD.Account","Online","CreditCard")
for(i in colsToFactors){
  data1[,i] = as.factor(data1[,i])
}

str(data1)
summary(data1)

data1 = data1[data1$Experience>0,]
summary(data1)


cor(data1[,c("Age","Experience","Income","Mortgage","CCAvg")])

data1$Age = NULL
# data1$Income = NULL


##### train test split
set.seed(123)
rows = 1:nrow(data1)
trainRows = sample(rows,round(0.7*nrow(data1)))

trainData = data1[trainRows,]
testData = data1[-trainRows,]

# summary(data1)

### Model building
model = glm(Personal.Loan~.-Experience,
            data=trainData,
            family = binomial(link = "logit"))

summary(model)

## Predictions
preds = predict(model,testData, type='response')
testData$preds = preds
#             preds
# actuals    0    1
#         0 1301   14
#         1   86   64

##Metrics
testData$preds2 = ifelse(testData$preds > 0.4,1,0)
table(testData$Personal.Loan,testData$preds2,dnn=c('actuals','preds'))


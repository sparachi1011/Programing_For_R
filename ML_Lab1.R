setwd("D:/Koushik/AP/ML_AP")
Test_Insurance<-read.csv("insurance.csv")
Test_Insurance

View(Test_Insurance)


##### Data Preprocess and validation::

hist(Test_Insurance$charges)

Insu_sqrt=sqrt(Test_Insurance$charges)

Insu_log=log(Test_Insurance$charges)


hist(Insu_sqrt)

hist(Insu_log)

summary(Insu_log)

plot(Test_Insurance$age,Insu_log)

cor(Test_Insurance$age,Insu_log)


cor(Test_Insurance[,c(1,3,4)])


####Preparing Test and Train data....


rows=1:nrow(Test_Insurance)

train_rows=sample(rows,round(length(rows)*0.8))
test_rows=rows[-train_rows]


train_data=Test_Insurance[train_rows,]
test_data=Test_Insurance[test_rows,]

####Model1::


model1 = lm(Test_Insurance$age~Test_Insurance$charges,data = Test_Insurance)
model1 = lm(train_data$age~train_data$charges,data = train_data)

model1 = lm(Insu_log ~.,data = train_data)

plot(model1)
summary(model1)

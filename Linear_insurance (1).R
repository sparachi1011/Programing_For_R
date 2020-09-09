setwd("C:/Users/phsivale/Documents/Trainings/Linear Regression/")

insurance = read.csv("insurance.csv")
head(insurance)

### pre checks 

str(insurance)
summary(insurance)
### target variable should follow normal distribution 

hist(insurance$charges)

### target variable is right skewed 
## apply transformation 

insurance$log_charges = log( insurance$charges)

hist(insurance$log_charges)
# hist((insurance$charges)**0.333)


#hist(sqrt(insurance$charges))
# plot(insurance$age[insurance$smoker=='no'],insurance$log_charges[insurance$smoker=='no'])
# plot(insurance$age[insurance$smoker=='yes'],insurance$log_charges[insurance$smoker=='yes'])

library(ggplot2)
ggplot(data=insurance)+
  geom_boxplot(aes(y=log_charges, x = smoker))+
  facet_grid(.~region)

### linear relationship b/w input and target 

plot(insurance$age, insurance$log_charges)
plot(insurance$age , insurance$charges)

### correlation b/w input and target variable

cor(insurance$age, insurance$log_charges) ## Correlation with target variable 


### multicollinearity ( input variables are correlated with each other)
cor(insurance[,c(1,3,4)])
cor( insurance$age, insurance$bmi) ### cor with input variable ( not desirable)
 
### Normality
hist(insurance$bmi)
hist(insurance$age)
hist(insurance$children)

## Imapct of smokers on insurance premiums
library(ggplot2)
ggplot( insurance, aes( smoker, log_charges)) + geom_boxplot()

### MOdel bulding  
insurance$charges = NULL 

## train and test set split 

set.seed(675)

ids = sample( nrow(insurance), nrow(insurance)*0.8)

train = insurance[ids,]
test = insurance[-ids,]

## model 

lin_model = lm(log_charges ~ . , data=train)

summary(lin_model)
plot(lin_model)

lin_model = lm(log_charges ~ . , data=train)
## Test the model 

test$pred = predict(lin_model, newdata=test )

summary(lin_model)

### RMSE 

test$error = test$log_charges - test$pred

test$error_sq = test$error ** 2

rmse = sqrt(mean(test$error_sq))
rmse

summary(test$log_charges)
(0.43/9.178)*100


###### Diagnosis #####################
### select only a few variable 
fit = lm(log_charges  ~  . , data=train)

plot(fit)

#### correlation check or Multicollinearity 
summary(fit)
names(fit)

fit$coefficients

head(fit$residuals)

### check for normality of errors 
hist(fit$residuals)

### cobine bmi and age into a new variable 
 
train$age_bmi = train$bmi/train$age

ggplot( train, aes( age_bmi, log_charges)) + geom_point()
 
#### final model with ratio of age and bmi 
 
fit = lm( log_charges ~ . -bmi -age-region-bmi.age,  data=train)
summary(fit) 

plot(fit)

### You can also try something like this
train$bmi.age = log( train$bmi*train$age)
hist(sqrt(train$bmi.age))
ggplot( train, aes(sqrt(bmi.age), log_charges)) + geom_point()


fit = lm( log_charges ~ . -bmi -age -age_bmi, data=train)

summary(fit)

plot(fit)

#### Other things to do
## Check for outliers mean +/- 3 sd
## Check for other features that can be engineered








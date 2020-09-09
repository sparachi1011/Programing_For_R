library(MASS)
data(Boston)
?Boston
Boston
summary(Boston$medv)

##log or sqrt transformation
hist(Boston$medv)
hist(log(Boston$medv))
hist(sqrt(Boston$medv))
summary(log(Boston$medv))
summary(sqrt(Boston$medv))

Boston$medv_sqrt = sqrt(Boston$medv)
#### Simple Linear Regression with Lstat

plot(Boston$lstat,Boston$medv)
cor(Boston$lstat,Boston$medv)

# abline(mod1)

plot(Boston$lstat,Boston$medv_sqrt)
cor(Boston$lstat,Boston$medv_sqrt)

Boston$lstat_sqrt = sqrt(Boston$lstat)

#### Handling Outliers

hist(Boston$medv_sqrt)
summary(Boston$medv_sqrt)

mean(Boston$medv_sqrt) + 3 * sd(Boston$medv_sqrt)
mean(Boston$medv_sqrt) - 3 * sd(Boston$medv_sqrt)

### train test split
set.seed(123)
# sample(rows,10)
rows= 1:nrow(Boston)
train_rows = sample(rows,round(length(rows)*0.7))
test_rows = rows[-train_rows]

train_data = Boston[train_rows,]
test_data = Boston[test_rows,]

### Model 1
mod1 = lm(medv~lstat,data=train_data)
mod1 = lm(medv_sqrt~lstat,data=train_data)
plot(train_data$lstat,train_data$medv)
plot(train_data$lstat,train_data$medv_sqrt)
summary(mod1)
abline(mod1)

### Model 2
mod2 = lm(medv_sqrt~lstat_sqrt,data=train_data)
plot(train_data$lstat_sqrt,train_data$medv_sqrt)
summary(mod2)
abline(mod2)


mean(Boston$lstat_sqrt) + 3 * sd(Boston$lstat_sqrt)
mean(Boston$lstat_sqrt) - 3 * sd(Boston$lstat_sqrt)
summary(Boston$lstat_sqrt)


##### mod3
mod3 = lm(medv_sqrt~.-lstat-medv-age-indus,data=train_data)
summary(mod3)


#### prediction
preds = predict(mod3, test_data)

test_data$preds = preds
test_data$predsFinall = preds**2


## Error or RMSE
sqrt(mean((test_data$predsFinall - test_data$medv)**2))
sqrt(mean((22.025 - test_data$medv)**2))



setwd('D:/Koushik/AP/ML_AP')
data1<-read.csv('UniversalBank.csv')
data1=data1[data1$Personal.Loan==1,]
#View(data1)

#data1<- (data1[,-c(1,2,3,5,9)])
data1$Personal.Loan=NULL
data1$ZIP.Code=NULL
data1$ID=NULL
data1$Experience=NULL
data1$Education=NULL
data1$Mortgage=NULL
View(data1)


str(data1)
data1$Family=as.factor(data1$Family)
#data1$Education=as.factor(data1$Education)

summary(data1)
View(data1)
str(data1)
names(data1)

install.packages('dummies')
### Convert Cat to numeric
library(dummies)
data1_dummies = dummy.data.frame(data1)
names(data1_dummies)
data1_dummies = data1_dummies[,-c(6,10)]
View(data1_dummies)


data1$Family = as.factor(data1$Family)
data1$Education = as.factor(data1$Education)

summary(data1)
# data1$Experience[data1$Experience <0] = 0

View(cor(data1[,-c(4,6,8,9,10,11)]))

data1$Experience = NULL # High cor with Age
# data1$CCAvg = NULL # High cor with Income Will drop this later

### Convert Cat to numeric
library(dummies)
data1_dummies = dummy.data.frame(data1)
names(data1_dummies)
data1_dummies = data1_dummies[,-c(6,10)]


### Scaling
###Min max scaling
fnScaling = function(x){
  return((x-min(x))/(max(x)-min(x)))
}


for(i in 1:ncol(data1_dummies)){
  data1_dummies[,i] = fnScaling(data1_dummies[,i])
}
summary(data1_dummies)

### Kmeans clustering

clust =  kmeans(x=data1_dummies,centers = 4)

data1_dummies$clust = clust$cluster

clust$cluster ## CLUSTER ID
clust$centers ## Centroids
clust$betweenss ## 
mean(clust$withinss)/clust$betweenss ## IntraCluster/interCluster

data1_dummies$clust = NULL
##
withinByBetween = c()
for(i in 2:15){
  clust = kmeans(x=data1_dummies,centers = i)
  ##betweenByTotal = c(betweenByTotal,clust$betweenss/clust$totss)
  withinByBetween = c(withinByBetween, mean(clust$withinss)/clust$betweenss)
}
# kmeans(x=data1_dummies,centers = 5)
plot(2:15,withinByBetween,type = 'l')




View(mtcars)
summary(mtcars)



data("mtcars")
mtcars
rownames(mtcars) = 1:nrow(mtcars)

summary(mtcars)

fnScaling = function(x){
  return((x-min(x))/(max(x)-min(x)))
}


for(i in 1:ncol(mtcars)){
  mtcars[,i] = fnScaling(mtcars[,i])
}

## Convert all the features from categorical to dummies
## Not done in the class

## Distance Matrix
distmat = dist(as.matrix(mtcars))

View(as.matrix(distmat))

## hierarchical clustering
hierclust = hclust(distmat)
plot(hierclust)

clusterCut <- cutree(hierclust, h=3.0)#k=5
plot(clusterCut)
table(clusterCut)
mtcars$cluster = clusterCut

aggregate(mtcars,list(mtcars$cluster),mean)





####
data("USArrests")
USArrests
hc <- hclust(dist(USArrests))
plot(hc)

clusterCut = cutree(hc,k=3)
plot(hc)

hist()

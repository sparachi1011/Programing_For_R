df = read.csv('D:/Koushik/AP/ML_AP/Groceries_wide.csv')
library(arules)
df$id = as.factor(df$id)

library(reshape2)
df_new = dcast(df,id~product,length)
write.csv(df_new, 'C:/Users/phsivale/Documents/Trainings/Groceries_wide.csv')
rm(df)
### data
str(df_new)
df_new = read.csv('C:/Users/phsivale/Documents/Trainings/Groceries_wide.csv')


for(i in 1:ncol(df_new)){
  df_new[,i] = as.factor(df_new[,i])
}

str(df_new)

df_new$id = NULL

library(arules)

##Converting to Transactions Object
df_trans = as(df_new,'transactions')
inspect(head(df_trans,6))


##creating rules
# library(arules)
rules = apriori(df_trans,
                parameter = list(supp = 0.001,
                                 confidence=0.5,
                                 target ='rules'))#"frequent itemsets"

inspect(head(rules,20))

rulesdf = as(rules,'data.frame')

## Subsetting Rules
rules_beer = subset(rules,subset = rhs %pin% "beer")
inspect(rules_beer)

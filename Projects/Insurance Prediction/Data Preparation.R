install.packages("readr")
install.packages("sparse")
install.packages("ade4")
install.packages("data.table")
install.packages("Hmisc")
install.packages("stringi")
install.packages("Metrics")

library(readr)
library(Matrix)
library(ade4)
library(data.table)
library(caret)
library(Hmisc)
library(stringi)
library(Metrics)


set.seed(1)
setwd("/Users/akshayasuresh/MS_NEU/Spring2018/ADS/ADS midterm project/Prudential")


#Import Data
train = read.csv("/Users/akshayasuresh/MS_NEU/Spring2018/ADS/ADS midterm project/Prudential/train.csv")
test = read.csv("/Users/akshayasuresh/MS_NEU/Spring2018/ADS/ADS midterm project/Prudential/test.csv")
summary(train)

#List columns and their null counts for train
apply(train, 2, function(x) { sum(is.na(x)) })

#List columns and their null counts for test
apply(test, 2, function(x) {  sum(is.na(x)) })


#Make a vector with column names
col_names = colnames(train)
col_names

#Create empty vector for list of columns to drop
drop_list=c()


#Loop through train dataframe to check percentage of nulls.
#If greater than 60%, add column to drop_list
for(x in names(train)){
  row_count = nrow(train)
  null_count = sum(is.na(train[,x]))
  #print(null_count)
  ratio = (null_count/row_count)
  if(ratio>0.60){
    drop_list = c(drop_list, x)
    #Prints each column name as detected
    print(paste("Null percentage for ",x,"is",ratio))
  }
}

drop_list

#Creating a duplicate dataframe
train_2 = train

#Drop columns from train dataframe
for(x in drop_list){
  #train = subset(train,subset = -train[,x])
  train_2[,x] <- NULL
}


#Drop columns from test dataframe
for(x in drop_list){
  #train = subset(train,subset = -train[,x])
  test[,x] <- NULL
}

#Splitting based on variables

cat.var.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Family_Hist_1", paste("Medical_History_", c(2:9,11:14, 16:23, 25:31, 33:41), sep=""))
cont.var.names <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                    "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4")
disc.var.names <- c("Medical_History_1", paste("Medical_Keyword_", 1:48, sep=""))

train.cat <- train_2[, cat.var.names]
test.cat <- test[, cat.var.names]

train.cont <- train_2[, cont.var.names]
test.cont <- test[, cont.var.names]

train.disc <- train_2[, disc.var.names]
test.disc <- test[, disc.var.names]

#train.cat <- as.data.frame(lapply(train.cat, factor))
#test.cat <- as.data.frame(lapply(test.cat, factor))


str(train.cat)
str(train.cont)
str(train.disc)

str(test.cat)
str(test.cont)
str(test.disc)

#Replace missing categorical values with median
for(i in 3:ncol(train.cat)){
  train.cat[is.na(train.cat[,i]), i] <- median(train.cat[,i], na.rm = TRUE)
}
#
for(i in 3:ncol(test.cat)){
  test.cat[is.na(test.cat[,i]), i] <- median(test.cat[,i], na.rm = TRUE)
}

#Replace missing continuous values with mean
for(i in 1:ncol(train.cont)){
  train.cont[is.na(train.cont[,i]), i] <- mean(train.cont[,i], na.rm = TRUE)
}
#
for(i in 1:ncol(test.cont)){
  test.cont[is.na(test.cont[,i]), i] <- mean(test.cont[,i], na.rm = TRUE)
}

#Replace missing discrete values with median
for(i in 1:ncol(train.disc)){
  train.disc[is.na(train.disc[,i]), i] <- median(train.disc[,i], na.rm = TRUE)
}
#
for(i in 1:ncol(test.disc)){
  test.disc[is.na(test.disc[,i]), i] <- median(test.disc[,i], na.rm = TRUE)
}

#Verify that nulls have been replaced
apply(train.cat, 2, function(x) { sum(is.na(x)) })
apply(train.cont, 2, function(x) { sum(is.na(x)) })
apply(train.disc, 2, function(x) { sum(is.na(x)) })

apply(test.cat, 2, function(x) { sum(is.na(x)) })
apply(test.cont, 2, function(x) { sum(is.na(x)) })
apply(test.disc, 2, function(x) { sum(is.na(x)) })

#Merge test and train categorical for purposes encoding
combo_cat = rbind(train.cat,test.cat)

#One-hot-encoding categorical variables

for (f in colnames(combo_cat)){
  df_all_dummy = acm.disjonctif(combo_cat[f])
  combo_cat[f] = NULL
  combo_cat = cbind(combo_cat, df_all_dummy)
}


train.cat.mod = combo_cat[1:59381,]
test.cat.mod = combo_cat[59382:79146,]

#For test
#for (f in colnames(test.cat)){
#  df_all_dummy = acm.disjonctif(test.cat[f])
#  test.cat[f] = NULL
#  test.cat = cbind(test.cat, df_all_dummy)
#}


#Merge files again
train_final = cbind(train.cat.mod,train.cont,train.disc,train_2$Response)
colnames(train_final)
colnames(train_final)[948]
colnames(train_final)[948] <- c("Response")
colnames(train_final)


#Merge test
test_final = cbind(test.cat.mod,test.cont,test.disc)
#colnames(test_final)[1]
#colnames(test_final)[1] <- c("Id")
colnames(test_final)

str(train_final)

#Splitting the data into test and train data from train data in order to find RMSE value
ind <- sample(2, nrow(train_final), replace = T, prob = c(0.8, 0.2))
train_sec <- train_final[ind==1,]
test_sec <- train_final[ind==2,]

str(test_sec)

test_vals <- test_sec$Response

str(test_vals)
max(test_vals)

#Cleaning ends here




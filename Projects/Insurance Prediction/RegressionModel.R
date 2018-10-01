#Next steps are specific to Linear Regression
#Reduce dimension using p-values in linear regression.

#Linear Regression with all the attributes
prelinearRegModel <- lm( formula = Response ~. ,
                         data = train_final)
summary(prelinearRegModel)
str(summary(prelinearRegModel))

#Finding the attribute names
colnames <- attr(prelinearRegModel$terms ,"term.labels")
colnames
length(colnames)

summary(prelinearRegModel)$coefficients[,4]

#P-value for the attributes
pValues <- summary(prelinearRegModel)$coefficients[,4]
pValues 
names(pValues)
length(pValues)

#Storing attribute names of P-values that are NA
NAPValues <- c()
flag = 0
for( x in colnames){
  for( y in names(pValues)){
    if(x == y){
      flag=1
    }
  }
  if(flag == 0){
    NAPValues = c(NAPValues, x)
  }
  flag = 0
}
NAPValues
length(NAPValues)

#Finding attributes with significant P-value
nsigPValues = c()

for(i in names(pValues)){
  if(pValues[i] >= 0.05 ){
    nsigPValues = c(nsigPValues,i)
  }
}
nsigPValues
length(nsigPValues)


#Preparing data for linear regression
for(x in nsigPValues){
  train_final[,x] <- NULL
}
length(train_final)
for(x in NAPValues){
  train_final[,x] <- NULL
}
str(train_final)
length(train_final)

#Fitting a model
linearRegModel <- lm( formula = Response ~ .,
                      data = train_final)
summary(linearRegModel)


#Find RMSE value = 1.99 using train data
try_predict <- round(predict( object = prelinearRegModel,
                              type = "response",
                              newdata = test_sec))
y_predict <- round(predict( object = linearRegModel,
                            type = "response",
                            newdata = test_sec))

str(y_predict)
mean(y_predict)
summary(y_predict)
rmse(test_vals,y_predict)


anova(prelinearRegModel, linearRegModel)

#Making a prediction
submission <- data.frame(Id=test$Id)
submission
submission$Response <- as.integer(round(predict( object = linearRegModel,
                                                 type = "response",
                                                 newdata = test_final)))
submission
summary(submission)

# As some predictions may be outside the range
submission[submission$Response<1, "Response"] <- 1
submission[submission$Response>8, "Response"] <- 8

cat("saving the submission file\n")
write_csv(submission, "linearRegression_submission.csv")
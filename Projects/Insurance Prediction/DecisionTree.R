###
### Decision Tree
model <- rpart(Response~., 
               data=train_sec,
               method="anova",
               control=rpart.control(minsplit=50,cp=0.001))

#plot the model
#install.packages("rattle")
#install.packages("rpart.plot")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(model)

#post(model,file = "Regression Tree.ps", title = "Regression Tree")

#predict
prediction <- round(predict(model,test_sec[,feature.names.test],type="vector"))

summary(prediction)

#confusing matrix
install.packages("caret")
install.packages("e1071")

library(caret)

#confusionMatrix
#confusionMatrix(prediction,test_vals)

#RMSE
RMSE <- sqrt(mean((test_vals-prediction)^2))
RMSE
###
###







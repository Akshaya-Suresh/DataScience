##Installing required packages
install.packages("rpart.plot")
install.packages("party")

##Using the pckgs
library ("rpart.plot")
library ("rpart")

##CODE:



Train_Data <- read.csv("/Users/akshayasuresh/MS NEU/Spring 2018/ADS/Assignment/Assignment05/Titanic train.csv")
Train_Data

Test_Data <- read.csv("/Users/akshayasuresh/MS NEU/Spring 2018/ADS/Assignment/Assignment05/Titanic test.csv")
Test_Data

Decision_Tree <- rpart(Survived ~ Pclass + Sex	+SibSp	+Parch +Fare	+Embarked ,method="class", data=Train_Data )
rpart.plot(Decision_Tree)
predict(object = Decision_Tree,newdata = Testing_Data, type="class")
table(predict(object = Decision_Tree,newdata = Testing_Data, type="class"))








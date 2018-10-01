###########################################################
######### Please fill the ??? with proper description (atleast 130 charaters for each)
######### for SVM function try different values to achieve better results

install.packages("caret")
install.packages("e1071")
# loading neccessary packages and dataset
library(caret)
library(e1071)
data(GermanCredit)
dataset = GermanCredit


#Data is being loaded here 
str(dataset)
dataset[,1:7] = as.data.frame(lapply(dataset[,1:7], scale))
str(dataset)

#When data is being sampled here, 200 data from the sample is chosen
sample_index = sample(1000, 200)
test_dataset = dataset[sample_index,]
train_dataset = dataset[-sample_index,]

model_afterTuning = svm(Class ~ ., kernel ="radial",cost=2 , gamma=0.5,data = train_dateset, scale = F)

#We are predicting our testdata set using the  simple model (Which is not tuned )
predictions_afterTuning <-  predict(model_afterTuning, test_dataset[-10])

#Plotting a confusion matrix for the table predicted above
table(test_dataset[,10], predictions_afterTuning)



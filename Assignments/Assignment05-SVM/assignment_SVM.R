###########################################################
######### Please fill the ??? with proper description (atleast 130 charaters for each)
######### for SVM function try different values to achieve better results


# loading neccessary packages and dataset
library(caret)
library(e1071)
data(GermanCredit)
dataset = GermanCredit
  

# ???
str(dataset)
dataset[,1:7] = as.data.frame(lapply(dataset[,1:7], scale))
str(dataset)

# ???
sample_index = sample(1000, 200)
test_dateset = dataset[sample_index,]
train_dateset = dataset[-sample_index,]

# ???
model = svm(Class ~ ., kernel = ???, cost = ???, gamma = ???, data = train_dateset, scale = F)

# ???
predictions <-  predict(model, test_dateset[-10])
# ???
table(test_dateset[,10], predictions)

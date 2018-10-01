## http://dni-institute.in/blogs/building-predictive-model-using-svm-and-r/

## Read data
termCrosssell <-read.csv(file="C:\\MY FILES\\Data\\Bank Marketing\\bank-full.csv",header=T)
 
## Explore data frame
names(termCrosssell)
 
 
## [1] "age" "job" "y"

## Dependent Variable
 
table(termCrosssell$y)
## 
##    no   yes 
## 36548  4640
table(termCrosssell$y)/length(termCrosssell$y)
## 
##        no       yes 
## 0.8873458 0.1126542
## Summary Statistics: Independent Variables
 
summary(termCrosssell$age)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   17.00   32.00   38.00   40.02   47.00   98.00
summary(termCrosssell$job)
##        admin.   blue-collar  entrepreneur     housemaid    management 
##         10422          9254          1456          1060          2924 
##       retired self-employed      services       student    technician 
##          1720          1421          3969           875          6743 
##    unemployed       unknown 
##          1014           330

svm.develop <- termCrosssell[sample(nrow(termCrosssell),
                      size=20000,
                      replace=TRUE),]
 
table(svm.develop$y)/length(svm.develop$y)
## 
##      no     yes 
## 0.88575 0.11425
svm.validate <- termCrosssell[sample(nrow(termCrosssell),
                      size=20000,
                      replace=TRUE),]
table(svm.validate$y)/length(svm.validate$y)
## 
##     no    yes 
## 0.8866 0.1134

#install.packages("e1071")
 
# Load SVM package
require(e1071)
## Loading required package: e1071
# Help 
#library(help=e1071)

svm.model <- svm(y~age+job,
                 data=svm.develop)
 
# Summary of the model
summary(svm.model)
 
## 
## Call:
## svm(formula = y ~ age + job, data = svm.develop)
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  radial 
##        cost:  1 
##       gamma:  0.07692308 
## 
## Number of Support Vectors:  4738
## 
##  ( 2453 2285 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  no yes

# Predict Target Label
svm.validate$Predicted_y <- predict(svm.model, 
                                    svm.validate[,-3])
 
# Compare Observed and Predicted
 table.svm <- table(pred = svm.validate$Predicted_y,
                    true = svm.validate$y)/length(svm.validate$y)

svm.simulate <- function(){
  gamma.value <- c(10^seq(-5,-1, by=1))
  cost.value <-c(10^seq(-3,1, by=1))
  svm.df <-data.frame()
    for(g in gamma.value){
      for(c in cost.value){
        # train model
        svm.model <- svm(y~age+job,
                         kernel="radial",
                         gamma=g,
                         cost=c,
                         data=svm.develop)
        # validate model
        svm.validate$Predicted_y <- predict(svm.model, 
                                    svm.validate[,-3])
        # Compare Observed and Predicted
        table.svm <- prop.table(table(pred = svm.validate$Predicted_y,
                    true = svm.validate$y)/length(svm.validate$y),1)
        in.df <- data.frame(gamma=g,
                  cost=c,
                  accuracy=table.svm[4] )
        svm.df <- rbind(svm.df,in.df)
      }
    }
  svm.df
}
 
svm.simulate.out <-svm.simulate()
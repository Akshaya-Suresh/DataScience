##Intalling Packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("xlsx")

##Using the required libraries
library("xlsx")
library (rpart)
library (rpart.plot)


data_file <- read.xlsx("/Users/akshayasuresh/MS NEU/Spring 2018/ADS/Assignment/Assignment05/Energy Efficiency ENB2012_data.xlsx",1)
data_file

energy_tree <- rpart(Y1 ~ X1	+X2	+X3	+X4	+X5+	X6+	X7+	X8 ,method="anova", data=data_file)
rpart.plot(energy_tree)

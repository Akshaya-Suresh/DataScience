install.packages("glmnet")
install.packages("ridge")
install.packages("mvtnorm")
library(glmnet)
library(ridge)
library(mvtnorm)


#Penalize the magnitude of coefficients
degArray <- seq(60, 300, by=4)
degArray
x <- c(pi*degArray/180)
x

l = length(x)
set.seed(10)
addValues <- rnorm(l, mean = 0, sd=0.15)
addValues
y <- c(sin(x) + addValues)
y
df <- data.frame(x = x , y = y )
df
plot(x,y)

#Append dataframe for each power upto 15

for(i in seq(2,15)){
  powX <- x^i
  df[i+1] <- c(powX)
}
colnames(df) <- c("x","y","x_2","x_3","x_4","x_5","x_6",
                  "x_7","x_8","x_9","x_10","x_11","x_12","x_13","x_14","x_15")
df

#Linear Regression
#can include a no of independent variables 

linear.fit <- lm( y ~ x+ x_2+ x_3+ x_4+ x_5+ x_6+ x_7+ x_8+ x_9+ x_10+
                    x_11+ x_12+ x_13+ x_14+ x_15, data = df )
summary(linear.fit)
print(linear.fit)
print(summary(linear.fit))
residuals.lm(linear.fit)

#Ridge Regression

alphaRidge <- c(1e-15, 1e-10, 1e-8, 1e-4, 1e-3,1e-2, 1, 5, 10, 20)
ridge.fit <- linearRidge( y ~ x+ x_2 + x_3+ x_4+ x_5+ x_6+ x_7+ x_8+ 
                    x_9+ x_10+ x_11+ x_12+ x_13+ x_14+ x_15, data = df,
                          nPCs = 15)
summary(ridge.fit)
print(ridge.fit)


#Lasso Regression

alphaLasso <- c(1e-15, 1e-10, 1e-8, 1e-4, 1e-3,1e-2, 1, 5, 10)
lasso.fit <- Lasso(df,y, alpha=1)

#Exercises 1 
#1
1:20
20:1
c(1:20,19:1)
tmp <- c(4,6,3)
rep(tmp,10)
rep(tmp,1,31 )
rep(tmp, times=c(10,20,30))

#2
temp <- seq(3,6,by= 0.1)
exp(temp)*cos(temp)

#3
(0.1^seq(3,36,by=3))*(0.2^seq(1,34,by=3))
(2^(1:25))/(1:25)

#4
tmp <- 10:100
sum(tmp^3+4*tmp^2)
tmp <- 1:25
sum((2^tmp)/tmp + 3^tmp/(tmp^2))

#4
paste("label", 1:30)
paste("fn", 1:30,sep="")

#5
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
yVec[-1] - xVec[-length(xVec)]
sin(yVec[-length(yVec)]) / cos(xVec[-1])
xVecLen <- length(xVec)
xVec[-c(xVecLen-1,xVecLen)] + 2*xVec[-c(1,xVecLen)] - xVec[-c(1,2)]
sum(exp(-xVec[-1])/(xVec[-length(xVec)]+10))

#6
yVec[yVec>600]
which(yVec>600) 
xVec[yVec>600]
sqrt(abs(xVec-mean(xVec)))
sum( yVec>max(yVec)-200 )
sum(xVec%%2==0)
xVec[order(yVec)]
yVec[c(T,F,F)]

#7
1+sum(cumprod(seq(2,38,b=2)/seq(3,39,b=2)))

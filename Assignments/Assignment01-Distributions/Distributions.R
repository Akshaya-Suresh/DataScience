#Gaussian Distribution (Normal Distribution)  
#------------------------------------------

#Consider the grades in a class are normally distributed with a mean of 75 and with a standard deviation of 5.

#percentage of students scoring higher than 70
pnorm(q=70,mean=75,sd=5,lower.tail = FALSE)

#percentage of students scoring lesser than 70 P(x<=70)
pnorm(q=70,mean=75,sd=5,lower.tail = TRUE)

#P(Z>=1)
pnorm(q=1,mean=0,sd=1,lower.tail = FALSE)

#finding
qnorm(p=.25, mean = 75, sd=5, lower.tail = TRUE)

#to Plot normal distribution graph
x <- seq(from=55, to=95, by=0.25)
xdens<- dnorm(x,mean = 75,sd=5)
plot(x, dens)

#to make the graph into a line
plot(x, dens,type="l")

#to draw a line at 75
abline(v=75)

#Exponential Distribution
#------------------------------------------

#the average checkout time at a super market is 10 minutes, find the probablity that the checkout takes less than 15 minutes.
#mean = 10, requred time = 15
mean = 10
x<-seq(0,15,by=1)
y<-dexp(x, rate = 1/10)
plot(x,y)
lines(x,y)

#Poisson Distribution
#------------------------------------------

#What is the number of patients arriving in an emergency roomin 24 hours. Given the mean is 15 patients arriving through out the day
#dpois is used for finding the probability density function of X, f(x)
x<-seq(0,24, by=1)
x
y<-dpois(x, lambda = 15,FALSE)
y
plot(x,y)
lines(x,y)

#Binomial Distribution
#------------------------------------------

#Flipping a coin will result in either heads or tails, so the probability of success is 1/2.So for 15 trails and x between 10-14
#To Plot binomial distribution graph

x<-seq(0,15,by=1)
y<-dbinom(x,15,1/2)
plot(x,y)
lines(x,y)



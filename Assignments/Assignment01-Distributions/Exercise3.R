#Exercises 3 

#1
tmpFn1 <- function(xVec)
{
  xVec^(1:length(xVec))
}
tmpFn1(1:3)
tmpFn2 <- function(xVec)
{
  n <- length(xVec)
  (xVec^(1:n))/(1:n)
}
tmpFn2(1:3)
tmpFn3 <- function(x, n)
{
  1 + sum((x^(1:n))/(1:n))
}
tmpFn3(1:2,2)

#2
tmpFn <- function(xVec) {
  n <- length(xVec)
  ( xVec[ -c(n-1,n) ] + xVec[ -c(1,n) ] + xVec[ -c(1,2) ] )/3
}
tmpFn(1:3)

#3
tmpFn <- function(x) {
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x < 2, x+3, x^2 + 4*x - 7))
}
tmp <- seq(-3, 3, len=100)
plot(tmp, tmpFn(tmp), type="l")

#4
tmpFn <- function(mat) 
{
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  mat 
}
tmpFn(5)

#5
tmp <- diag(2, nr = 5)
tmp[abs(row(tmp) - col(tmp)) == 1] <- 1 
tmp
tmpFn <- function(n, k)  {
  tmp <- diag(k, nr = n)
  tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
  tmp
}

#6
quadrant <- function(alpha) {
  1 + (alpha%%360)%/%90
}
quadrant(100)
quadrant(400)

#7
weekday <- function(day, month, year) {
  month <- month - 2
  if(month <= 0) {
    month <- month + 12
    year <- year - 1
  }
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}
weekday(19,09,1995)
c( weekday(27,2,1997), weekday(18,2,1940), weekday(21,1,1963) )

#8
testLoop <- function(n)
{
  xVec <- rep(NA, n-1)
  xVec[1] <- 1
  xVec[2] <- 2
  for( j in 3:(n-1) )    
    xVec[j] <- xVec[j-1] + 2/xVec[j-1]
  xVec
}
testLoop(5)

testLoop2 <- function(yVec)
{
  n <- length(yVec)
  sum( exp(seq(along=yVec)) )
}
testLoop2(0)

#9
quadmap <- function(start, rho, niter)
{
  xVec <- rep(NA,niter)
  xVec[1] <- start
  for(i in 1:(niter-1)) {
    xVec[i + 1] <- rho * xVec[i] * (1 - xVec[i])
  }
  x
}

quad2 <- function(start, rho, eps = 0.02)
{
  x1 <- start
  x2 <- rho*x1*(1 - x1)
  niter <- 1
  while(abs(x1 - x2) >= eps) {
    x1 <- x2
    x2 <- rho*x1*(1 - x1)
    niter <- niter + 1
  }
  niter 
}

#10
tmpAcf <- function(xVec)
{
  xc <- xVec - mean(xVec)
  denom <- sum(xc^2)
  n <- length(x)
  r1 <- sum( xc[2:n] * xc[1:(n-1)] )/denom
  r2 <- sum( xc[3:n] * xc[1:(n-2)] )/denom
  list(r1 = r1, r2 = r2)
}


tmpAcf <- function(x, k)
{
  xc <- x - mean(x)
  denom <- sum(xc^2)
  n <- length(x)
  tmpFn <- function(j){ sum( xc[(j+1):n] * xc[1:(n-j)] )/denom }
  c(1, sapply(1:k, tmpFn))
}
tmpAcf(1:4,2)


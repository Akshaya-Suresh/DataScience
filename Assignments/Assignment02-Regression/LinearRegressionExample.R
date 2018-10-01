Factor1 <- c(41.9,
       43.4,
       43.9,
       44.5,
       47.3,
       47.5,
       47.9,
       50.2,
       52.8,
       53.2,
       56.7,
       57.0,
       63.5,
       65.3,
       71.1,
       77.0, 
       77.8
)


Factor2 <- c (29.1,
        29.3,
        29.5,
        29.7,
        29.9,
        30.3,
        30.5,
        30.7,
        30.8,
        30.9,
        31.5,
        31.7,
        31.9,
        32.0,
        32.1,
        32.5,
        32.9
)

Yield <- c(251.3,
       251.3,
       248.3,
       267.5,
       273.0,
       276.5,
       270.3,
       274.9,
       285.0,
       290.0,
       297.0,
       302.5,
       304.5,
       309.3,
       321.7,
       330.7,
       349.0
)

d <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
       1,
       1
)



X <- cbind(d , a , b)
X


XTranspose <- t(X)
#XTranspose

XTransposeX <-  XTranspose %*% X
#XTransposeX
InverseXTransposeX <- solve(XTransposeX)


XTransposey <-  XTranspose %*% y
#XTransposey



Beta <- InverseXTransposeX %*% XTransposey
Beta



b0 <- (-153.5)
b1 <- Factor1*1.24
b2 <- Factor2*12.08

#FittedValue

FittedValue <- b0 + b1 +b2
FittedValue 

#Residual

Residual <-  y - FittedValue 
Residual

# Computing the value of F0

yTranspose <- t(y)
#yTranspose

inverseXTransposeX <- solve(XTransposeX)
#inverseXTransposeX

hInter <- X %*% inverseXTransposeX
H <- hInter %*% XTranspose
H

k <- 2
n <- 17
J <- matrix(1, 17, 17)
Jdividen <- J/n
#Jdividen
HMinusJdividen <- H - Jdividen

HMinusJdivideny <- HMinusJdividen %*% y

SSr <- yTranspose %*% HMinusJdivideny
SSr

I <- matrix(0,17,17)
diag(I) <- 1
#I
IMinusH <- I - H
IMinusHy <- IMinusH %*% y
SSe <- yTranspose %*% IMinusHy
SSe

MSr <- SSr/ k
MSr

MSe <- SSe / (n-(k+1))
MSe

F0 <- MSr / MSe
F0


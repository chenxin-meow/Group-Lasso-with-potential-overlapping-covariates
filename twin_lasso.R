rm(list=ls())
source("lasso_model.R")
source("cv.R")
source("genPolyMatrix.R")


############################################
## Experiment 1
############################################

##-------------------------------
## Construct two datasets 
##-------------------------------
n=100; p=10

# Dataset 1
set.seed(11)
X1=matrix(rnorm(n*p), n, p)
X1=X1[order(X1[,1]),]
x1=X1[,1]
beta1.actual = 1:p/p
y1.model = X1%*%beta1.actual
y1 = y1.model + rnorm(n,0,0.5) # add noise to y1

# Dataset 2
set.seed(12)
X2=matrix(rnorm(n*p), n, p)
X2=X1[order(X2[,1]),]
x2=X2[,1]
beta2.actual = beta1.actual
beta2.actual[1:(p/2)]=1
y2.model = X2%*%beta2.actual
y2 = y2.model + rnorm(n,0,0.5) # add noise to y2

# Actual beta
beta1.actual
beta2.actual


##----------------------------------------------------
# Cross-Validation (k-fold) to find the best gamma
##----------------------------------------------------
gamma.len=10 # create gamma.list
gamma.list=rep(NA,gamma.len)
gamma.list[1]=0
for(j in 2:gamma.len) gamma.list[j]=10^(j-5)
(cv.gamma.sort=cv.gamma(X1,y1,X2,y2,k=5,gamma.list))

# select best gamma
(gamma.best=cv.gamma.sort$gamma.list[1])


twin_model0=twin_lasso(X1,y1,X2,y2,0)
twin_model0$beta1.fit
twin_model0$beta2.fit

##----------------------------------------------------
# Lasso model (for (X1,y1) and (X2,y2))
##----------------------------------------------------
twin_model=twin_lasso(X1,y1,X2,y2,gamma.best)
list(beta1.fit=twin_model$beta1.fit, beta1.actual=beta1.actual)
list(beta2.fit=twin_model$beta2.fit, beta2.actual=beta2.actual)




############################################
## Experiment 2
############################################

##-------------------------------
## Construct two datasets 
##-------------------------------
n=100; p=5

#-----------------------------------------------
## Dataset 1
#-----------------------------------------------
set.seed(11)
x1 = seq(-2,2,length.out=n)
X1 = matrixX(x1,p)
beta1.actual = rep(1,p)
y1.model = X1%*%beta1.actual
y1 = y1.model + rnorm(n,0,100) # add noise to y1


# Dataset 2
set.seed(12)
x2 = seq(-2,2,length.out=n)
X2 = matrixX(x2,p)
beta2.actual = rep(1,p)
beta2.actual[(p/2+1):p]=rep(2,p/2)# a little change in beta2
y2.model = X2%*%beta2.actual
y2 = y2.model + rnorm(n) # add noise to y2

# Actual beta
beta1.actual
beta2.actual

##----------------------------------------------------
# Cross-Validation (k-fold) to find the best gamma
##----------------------------------------------------
gamma.len=10 # create gamma.list
gamma.list=rep(NA,gamma.len)
gamma.list[1]=0
for(j in 2:gamma.len) gamma.list[j]=10^(j-5)
(cv.gamma.sort=cv.gamma(X1,y1,X2,y2,k=5,gamma.list))

# select best gamma
(gamma.best=cv.gamma.sort$gamma.list[1])


twin_model0=twin_lasso(X1,y1,X2,y2,0)
twin_model0$beta1.fit
twin_model0$beta2.fit

##----------------------------------------------------
# Lasso model (for (X1,y1) and (X2,y2))
##----------------------------------------------------
twin_model=twin_lasso(X1,y1,X2,y2,gamma.best)
list(beta1.fit=twin_model$beta1.fit, beta1.actual=beta1.actual)
list(beta2.fit=twin_model$beta2.fit, beta2.actual=beta2.actual)



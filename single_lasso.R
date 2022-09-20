rm(list=ls())
source("lasso_model.R")
source("cv.R")
source("genPolyMatrix.R")





############################################
## Experiment 1
############################################

##-------------------------------
# Construct one dataset 
##-------------------------------
n=100; p=10

# 1 Dataset 
set.seed(11)
X1=matrix(rnorm(n*p), n, p)
X1=X1[order(X1[,1]),]
x1=X1[,1]
beta1.actual = 1:p/p
y1.model = X1%*%beta1.actual
y1 = y1.model + rnorm(n,0,0.5) # add noise to y1

##------------------------------------------------
## Different lambda - graph illustration
##------------------------------------------------
lambda.len=10 # create lambda.list
lambda.list=rep(NA,lambda.len)
lambda.list[1]=0
for(j in 2:lambda.len) lambda.list[j]=10^(j-5)

pdf(file = "image/single-lasso-lambda-1.pdf",width = 10, height = 6, family="Times")
par(mfrow=c(2,5)) 
lambda.select = lambda.list
for (i.lambda in 1:length(lambda.select)){
  lambda.plot = lambda.select[i.lambda]
  model.plot = single_lasso(X1,y1,lambda.plot)
  plot.single(x1,y1,y1.model,model.plot$y.fit,lambda.plot,legend=FALSE)
  }
dev.off()


##----------------------------------------------------
# Cross-Validation (k-fold) to find the best lambda
##----------------------------------------------------
(lambda.best = cv.lambda(X1,y1,k=10,lambda.list)[1])


##----------------------------------------------------
# Lasso model (for (X1,y1))
##----------------------------------------------------
model1 = single_lasso(X1,y1,lambda.best)
model1$beta.fit
pdf(file = "image/single-lasso-1.pdf",width = 10, height = 6, family="Times")
plot.single(x1,y1,y1.model,model1$y.fit,lambda=lambda.best)
dev.off()




############################################
## Experiment 2
############################################

##-------------------------------
# Construct one dataset 
##-------------------------------
n=100; p=5

# 1 Dataset 
set.seed(11)
x1 = seq(-2,2,length.out=n)
X1 = matrixX(x1,p)
beta1.actual = rep(1,p)
y1.model = X1%*%beta1.actual
y1 = y1.model + rnorm(n,0,0.5) # add noise to y1


##------------------------------------------------
## Different lambda - graph illustration
##------------------------------------------------
lambda.len=10 # create lambda.list
lambda.list=rep(NA,lambda.len)
lambda.list[1]=0
for(j in 2:lambda.len) lambda.list[j]=10^(j-5)

pdf(file = "image/single-lasso-lambda-2.pdf",width = 10, height = 6, family="Times")
par(mfrow=c(2,5)) 
lambda.select = lambda.list
for (i.lambda in 1:length(lambda.select)){
  lambda.plot = lambda.select[i.lambda]
  model.plot = single_lasso(X1,y1,lambda.plot)
  plot.single(x1,y1,y1.model,model.plot$y.fit,lambda.plot,legend=FALSE)
}
dev.off()

##----------------------------------------------------
# Cross-Validation (k-fold) to find the best lambda
##----------------------------------------------------
(lambda.best = cv.lambda(X1,y1,k=10,lambda.list)[1])


##----------------------------------------------------
# Lasso model (for (X1,y1))
##----------------------------------------------------
model1 = single_lasso(X1,y1,lambda.best)
model1$beta.fit
pdf(file = "image/single-lasso-2.pdf",width = 10, height = 6, family="Times")
plot.single(x1,y1,y1.model,model1$y.fit,lambda=lambda.best)
dev.off()



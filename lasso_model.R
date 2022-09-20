

# cost function: squared error + L1 regularization
cost <- function(beta, X, y, lambda) {
  sum( (X %*% beta - y)^2 ) + lambda*sum(abs(beta))
}


# training Lasso for single group
single_lasso <- function(X,y,lambda){
  n=nrow(X); p=ncol(X)
  #beta0 <- rep(1,p)
  beta0 = lm(y~X-1)$coefficients
  beta.fit = optim(beta0, fn=cost, X=X, y=y, lambda=lambda, method = "BFGS")$par
  y.fit = X%*%beta.fit # fitted values
  y.res = y-y.fit # residuals
  rrs = sum(y.res^2)/(n-p) # Residual sum of squares
  
  return(list(beta.fit=beta.fit, y.fit=y.fit, rrs=rrs))
}



# plot 
plot.single = function(x,y,y.model,y.fit,lambda,legend=TRUE){
  plot(x,y,xlab=bquote(italic('lambda='~.(lambda))),
       pch=16,col="gray") # data-points
  lines(x,y.model,type='l',lty=2,col=2,lwd=2) # true model
  #points(x,y.fit,pch=2,col=4,lwd=2) # fitted value
  lines(x, y.fit, lty=1, col=4, lwd=2) 
  
  if (legend==TRUE){
    legend("top", c("Data", "Actual relationship", "Predicted relationship"), 
           lty=c(NA,2,1), pch=c(16,NA,NA),
           col=c("gray",2,4),lwd=c(2,2,2))
  }
}



# cost function: 2 datasets
cost2 <- function(beta, X1, y1, X2, y2, gamma, lambda1=0, lambda2=0) {
  p=ncol(X1)
  sum( (X1 %*% beta[1:p] - y1)^2 ) + sum( (X2 %*% beta[(p+1):(2*p)] - y2)^2 )+
    gamma*sum(abs(beta[1:p]-beta[(p+1):(2*p)]))+lambda1*sum(abs(beta[1:p]))+
    lambda2*sum(abs(beta[(p+1):(2*p)]))
}

# training Lasso for two groups
twin_lasso <- function(X1,y1,X2,y2,gamma){
  n=nrow(X1); p=ncol(X1)
  beta0 <- rep(0,2*p)
  beta.fit = optim(beta0, fn=cost2, X1=X1, y1=y1, X2=X2, y2=y2, gamma=gamma,
                   lambda1=0, lambda2=0, method = "BFGS")$par
  y1.fit = X1%*%beta.fit[1:p]
  y2.fit = X2%*%beta.fit[(p+1):(2*p)] # fitted values
  y1.res = y1-y1.fit 
  y2.res = y2-y2.fit # residuals
  rrs1 = sum(y1.res^2)/(n-p)
  rrs2 = sum(y2.res^2)/(n-p) # Residual sum of squares
  
  return(list(beta1.fit=beta.fit[1:p], beta2.fit=beta.fit[(p+1):(2*p)],
              y1.fit=y1.fit, y2.fit=y2.fit, rrs1=rrs1, rrs2=rrs2))
}



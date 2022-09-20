source("lasso_model.R")

# select lambda in single lasso
cv.lambda <- function(X,y,k=5,lambda.list){
  n <- nrow(X) # number of data points
  bucket_size <- floor(n/k) # size of each CV bucket
  # randomized indices to later make CV buckets
  randomized_indices <- sample(n)
  
  bucket_losses <- rep(0,k)
  cv_errors <- rep(0,length(lambda.list))
  
  for (i in 1:length(lambda.list)) {
    # iterate over CV buckets
    for (j in 1:k) {
      # compute this CV bucket
      if (j < k) {
        hold_out_idx <- randomized_indices[(1+(j-1)*bucket_size):(j*bucket_size)]
      } else {
        hold_out_idx <- randomized_indices[(1+(j-1)*bucket_size):n]
      }
      # train smoothing spline on all data EXCEPT this CV bucket
      beta0=lm(y~X-1)$coefficients # intial beta, lambda=0
      betafit = optim(beta0, fn=cost, X=X[-hold_out_idx,], y=y[-hold_out_idx], 
                      lambda=lambda.list[i], method = "BFGS")$par
      
      # evaluate error on held-out data
      yfit = c(X[hold_out_idx,]%*%betafit)
      yobs <- y[hold_out_idx]
      squared_loss_bucket <- mean((yobs - yfit)^2)
      
      # record loss on this bucket
      bucket_losses[j] <- squared_loss_bucket
    }
    cv_error <- mean(bucket_losses)
    cv_errors[i] <- cv_error
  }
  lambda.list[order(cv_errors)]
  
}




#####----------------------------------------------------------


# select gamma in twin lasso
cv.gamma <- function(X1,y1,X2,y2,k=5,gamma.list){
  n <- nrow(X1) # number of data points
  bucket_size <- floor(n/k) # size of each CV bucket
  # randomized indices to later make CV buckets
  randomized_indices <- sample(n)
  
  bucket_losses <- rep(0,k)
  cv_errors <- rep(0,length(gamma.list))
  
  for (i in 1:length(gamma.list)) {
    # iterate over CV buckets
    for (j in 1:k) {
      # compute this CV bucket
      if (j < k) {
        hold_out_idx <- randomized_indices[(1+(j-1)*bucket_size):(j*bucket_size)]
      } else {
        hold_out_idx <- randomized_indices[(1+(j-1)*bucket_size):n]
      }
      # train smoothing spline on all data EXCEPT this CV bucket
      beta0=c(lm(y1~X1-1)$coefficients, lm(y2~X2-1)$coefficients) # intial beta, gamma=0
      betafit = optim(beta0, fn=cost2, X1=X1[-hold_out_idx,], y1=y1[-hold_out_idx],
                      X2=X2[-hold_out_idx,], y2=y2[-hold_out_idx], gamma=gamma.list[i],
                      lambda1=0, lambda2=0, method = "BFGS")$par
      
      
      # evaluate error on held-out data
      y1fit = X1[hold_out_idx,]%*%betafit[1:p]
      y2fit = X2[hold_out_idx,]%*%betafit[(p+1):(2*p)] 
      y1obs <- y1[hold_out_idx]
      y2obs <- y2[hold_out_idx]
      squared_loss_bucket <- mean((y1obs - y1fit)^2+(y2obs - y2fit)^2)
      
      # record loss on this bucket
      bucket_losses[j] <- squared_loss_bucket
    }
    cv_error <- mean(bucket_losses)
    cv_errors[i] <- cv_error
  }
  return(list(gamma.list=gamma.list[order(cv_errors)],
              cv_errors=sort(cv_errors)))
  
}

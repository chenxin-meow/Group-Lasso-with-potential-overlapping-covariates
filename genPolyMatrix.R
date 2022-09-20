

matrixX = function(x,p){
  
  # Generate a covariate matrix in polynomial orders
  # Input:  x(length=n), p=length of beta
  # Output: matrix X, size=n*p
  
  n=length(x)
  X=rep(1,n);
  for (j in 1:(p-1)){X=cbind(X,x^j)}
  
  X
}


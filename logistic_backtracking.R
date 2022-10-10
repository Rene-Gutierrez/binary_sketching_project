logistic_backtracking <- function(Z,
                                  b,
                                  g,
                                  p = g,
                                  a = 1,
                                  t = 0.5,
                                  c = 0.5,
                                  maxiter = 100){
  # Computes the 
  m <- g %*% p
  f <- c()
  ma <- a
  mf <- -Inf
  for(i in 1:maxiter){
    # Checks Armijos Condition
    f1  <- logistic_loglikelihood(Z = Z,
                                  b = b + a * p)
    f0  <- logistic_loglikelihood(Z = Z,
                                  b = b)
    mf  <- max(mf, f1)
    if(mf == f1){
      ma <- a
    }
    if(f1 - f0 >= a * c * m){
      break
    } else {
      a <- t * a
    }
  }
  ### Return step size
  return(list(ma = ma,
              mf = mf))
}
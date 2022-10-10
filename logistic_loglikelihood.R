logistic_loglikelihood <- function(Z,
                                   b){
  m <- Z %*% b
  l <- sum(log(plogis(q = m)))
  return(l)
}
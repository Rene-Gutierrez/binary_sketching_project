logistic_gradient <- function(Z,
                              b){
  m <- Z %*% b
  l <- as.vector(plogis(q = - m))
  g <- colSums(l * Z)
  return(g)
}
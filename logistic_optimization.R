logistic_optimization <- function(Z,
                                  b,
                                  a        = 1,
                                  tol      = 10^(-4),
                                  max_iter = 100){
  sf <- c()
  sa <- c()
  sg <- numeric(length = length(b))
  sb <- numeric(length = length(b))
  # Iterates until gradient condition is met
  for(i in 1:max_iter){
    # Computes the Gradient
    g <- logistic_gradient(Z = Z,
                           b = b)
    sg <- cbind(sg, g)
    # Checks the Condition
    if(sqrt(t(g) %*% g) < tol){
      break
    } else {
      out <- logistic_opt_step(Z = Z,
                               b = b,
                               g = g,
                               a = a)
      a   <- out$a * 2
      b   <- out$b
      f   <- out$f
      sa  <- c(sa, a)
      sf  <- c(sf, f)
      sb  <- cbind(sb, b)
    }
  }
  # Returns the Optimum and Optimizer
  return(list(b = b,
              f = f,
              sf = sf,
              sb = sb,
              sg = sg,
              sa = sa))
}
logistic_opt_step <- function(Z,
                              b,
                              g,
                              a = 1){
  # Computes the Step Size
  out <- logistic_backtracking(Z = Z,
                               b = b,
                               g = g,
                               a = a)
  # Updates
  b <- b + out$ma * g
  return(list(b = b,
              f = out$mf,
              a = out$ma))
}
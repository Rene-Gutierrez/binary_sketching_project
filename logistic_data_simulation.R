### Logistic Data Simulator

# Problem Dimensions
P <- 10
N <- 1000

# Simulates beta
b0 <- NA
if(is.na(b0) == TRUE){
  b  <- rnorm(n    = P,
              mean = 0,
              sd   = 1)
} else {
  b  <- rnorm(n    = P - 1,
              mean = 0,
              sd   = 1)
  b  <- c(b0, b)
}

# Simulates X
if(is.na(b0) == TRUE){
  X <- matrix(data = rnorm(n    = N * P,
                           mean = 0,
                           sd   = 1),
              nrow = N,
              ncol = P)
  X  <- t(t(X) - colMeans(X))
  Xp <- matrix(data = rnorm(n    = N * P,
                            mean = 0,
                            sd   = 1),
               nrow = N,
               ncol = P)
  Xp <- t(t(Xp) - colMeans(Xp))
} else {
  X <- matrix(data = rnorm(n    = N * (P - 1),
                           mean = 0,
                           sd   = 3),
              nrow = N,
              ncol = P - 1)
  X  <- t(t(X) - colMeans(X))
  X  <- cbind(rep(1, N), X) 
  Xp <- matrix(data = rnorm(n    = N * (P - 1),
                            mean = 0,
                            sd   = 3),
               nrow = N,
               ncol = P - 1)
  Xp <- t(t(Xp) - colMeans(Xp))
  Xp <- cbind(rep(1, N), Xp)
}
 

# Simulates the probabilities
pro  <- 1 / (1 + exp(- X %*% b))
prop <- 1 / (1 + exp(- Xp %*% b))

# Simulates
y   <- rbinom(n = N, size = 1, prob = pro)
yp  <- rbinom(n = N, size = 1, prob = prop)
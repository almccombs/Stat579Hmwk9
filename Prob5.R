#Problem 5
op <- par()

#Problem 5.a
polaroid <- function(x) {
  p <- length(x)
  r <- sqrt(sum(x^2))
  
  theta <- rep(0, p-1)
  den <- rep(0, p-2)
  
  theta[1] <- acos(x[1]/r)
  den[1] <- r
  
  for (i in 2:(p-1)) {
    den[i] <- den[i-1] * sin(theta[i-1])
    theta[i] <- acos(x[i]/den[i])
  }
  
  polar <- c(r, theta)
  return(polar)
}

x <- seq(from = 0, to = 10, by = 2)
polaroid(x)

#Problem 5.b
normalize <- function(vec) {
  den <- sqrt(sum(vec^2))
  output <- vec/den
}

#Problem 5.c
y <- matrix(rnorm(5000, mean = 0, sd = 1), nrow = 1000, ncol = 5)
zt <- apply(y, 1, normalize)
z <- t(zt)

ks.test(z,"punif",min=-1,max=1)
hist(z)

#Based on the Kolmogorov–Smirnov test, the z matrix is not uniformly distributed.  We can see this clearly in the histogram of values in the matrix.

#Problem 5.d
polarst <- apply(y, 1, polaroid)
polars <- t(polarst)

ks.test(polars[,1]^2, "pchisq", 5)

boxplot(polars[,2:5])

ks.test(polars[,2], "punif", min = 0, max = 2*pi)
ks.test(polars[,3], "punif", min = 0, max = pi)
ks.test(polars[,4], "punif", min = 0, max = pi)
ks.test(polars[,5], "punif", min = 0, max = pi)

#Multiple distributions
par(mfrow = c(3,3))
for (i in 1:9) {
  y <- matrix(rnorm(5000, mean = 0, sd = 1), nrow = 1000, ncol = 5)
  zt <- apply(y, 1, normalize)
  z <- t(zt)
  polarst <- apply(y, 1, polaroid)
  polars <- t(polarst)
  boxplot(polars[,2:5])
}
par(op)
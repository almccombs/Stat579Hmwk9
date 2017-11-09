#Problem 1

matfunc <- function(n,k) {
  diag(x = k, nrow = n, ncol = n)
}
matfunc(6,5)

rm(list = ls())

#Problem 2

tmpFn <- function(xVec) {
  ifelse(test = xVec < 0,
      yes = ((xVec^2) + (2*xVec) + 3),
      no = ifelse(test = 0 <= xVec & xVec < 2,
          yes = (xVec + 3),
          no = ((xVec^2) + (4* xVec) - 7)))
  }

xVec <- seq(from = -3, to = 3, by = .01)
yVec <- tmpFn(xVec)
plot(xVec, yVec, xlab = "Input value", ylab = "Function output", main = "tmpFn function", asp = 1)

rm(list = ls())

#Problem 3

gdc <- function(m,n) {
  firstm <- m
  firstn <- n
  r <- 1
  while (r != 0) {
    r <- m %% n
    m <- n
    n <- r
  }
print(c(firstm, firstn, m))
}

gdc(1420,95)

rm(list = ls())

#Problem 4

order.matrix <- function(mymat) {
ordvec <- sort(mymat)
indrow <- (rep(NA, length(ordvec)))
indcol <- (rep(NA, length(ordvec)))
for (i in 1:length(ordvec)) {
  rowcol <- which(mymat == ordvec[i], arr.ind = TRUE)
  indrow[i] <- rowcol[1,1]
  indcol[i] <- rowcol[1,2]
}
values <- data.frame(number = ordvec, rowindex = indrow, colindex = indcol)
return(values)
}

mymat <- matrix(rchisq(12, 1), nrow = 4)
order.matrix(mymat)

mymat <- matrix(rchisq(20, 1), nrow = 5)
order.matrix(mymat)

rm(list = ls())

#Problem 5

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
normalize <- function(mymat) {
  divisor <- rowSums(mymat)
  for (r in 1:nrow(mymat)) {
    for (c in 1:ncol(mymat)) {
      mymat[r,c] <- mymat[r,c]/divisor[r]
    }
  }
  return(mymat)
}

mymat <- matrix(5:13, nrow = 3)
normalize(mymat)

  #Problem 5.c





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




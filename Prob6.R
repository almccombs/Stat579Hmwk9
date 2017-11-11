#Problem 6

  #Problem 6.a
x <-c(3.91, 4.85, 2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, 0.52, 2.50)
theta <- seq(0,10,,100)

hm <- function(theta = theta, x = 3.91) (1-cos(x - theta))/(2*pi)
loghm <- function(theta, x) (-log(2*pi)+sum(log(1-cos(x-theta)^2)))
plot(theta,sapply(theta,loghm,x),type="l",ylab="LogL of HW distribution", xlim = c(-3.1415927, 3.1415927))
plot(theta,sapply(theta,loghm,x),type="l",ylab="LogL of HW distribution", xlim = c(0, 10))

  #Problem 6.b
optimize(function(theta) sapply(theta, loghm, x), interval = c(1,2), maximum = T)

  #Problem 6.c
ghm <- function(theta,x) sin(x-theta)/(1-cos(x-theta))
derghm <- function(theta,x) 1/(1-cos(x-theta))

loglik <- function(fun, derf, x0, eps, nlim,...) {
  iter <- 0
  repeat {
    iter <- iter + 1
    if(iter > nlim) {
      cat(" Iteration Limit Exceeded: Current = ",iter, fill = T)
      x1 <- NA
      break
    }
    x1 <- x0 - fun(x0,...)/derf(x0,...)
    if(abs(x0 - x1) < eps||abs(fun(x1,...))<1.0e-12)
      break
    x0 <- x1
    cat("\n****** Iter. No: ", iter, " Current Iterate = ", x1, fill=T)
  }
  return(x1)
}

loglik(ghm, derghm, 0, 0.00001, 100, x)

#This found the MLE value at -0.64: the local maximum near x = 0.

  #Problem 6.d
loglik(ghm, derghm, -2.0, 0.00001, 100, x)
loglik(ghm, derghm, -2.7, 0.00001, 100, x)

# At a starting value of -2.0, the first iteration found an MLE of -1.02, which is a local maximum near -2.0.  Later iterations found the same MLE as with starting point of 0 (i.e., -0.64).  At a starting value of 2.7, however, the first iteration found the MLE at -1.8165, and it took more iterations to find the MLE of -0.64.  This function bounces around a lot, so it's not surprising that the function finds local maxima and takes a while to settle.

#Problem 7



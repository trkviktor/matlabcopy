x <- c(0, 1, 2, 3)
y <- c(1, 2.6, 5.4, 2.71^3 - 0.1)
n <- length(x)

#y = a* e^b*x
#utána ln
#a b megbecsülés és c kiszámolása(ln(a))
NonLinReg <- function(X, Y) {
  Y <- log(Y)
  a <- CalcA(X, Y)
  b <- CalcB(X, Y)
  #Y <- exp(Y)
  plot(X, Y)
  lines(X, a * exp(b * X), type = "l", lty = 1, col = "red")
}

CalcA <- function(X, Y) {
  sumX <- Summary(X)
  sumY <- Summary(Y)
  sumXY <- Summary(X, Y)
  sumXSquared <- Summary(X, X)
  
  upper <- (sumXY * n) - (sumY * (-sumX))
  lower <- (sumXSquared * n) - (sumX * (-sumX))
  return(upper / lower)
}

CalcB <- function(X, Y) {
  sumX <- Summary(X)
  sumY <- Summary(Y)
  sumXY <- Summary(X, Y)
  sumXSquared <- Summary(X, X)
  
  upper <- (sumX * sumY) - (sumX * sumXY)
  lower <- (sumXSquared * n) - (sumX * (-sumX))
  return(upper / lower)
}

Summary <- function(X, Y) {
  sum <- 0
  
  if (missing(Y)) {
    for (i in seq_along(X)) {
      sum <- sum + X[i]
    }
    return(sum)
  }
  
  for (i in seq_along(X)) {
    sum <- sum + X[i] * Y[i]
  }
  return(sum)
}
NonLinReg(x, y)
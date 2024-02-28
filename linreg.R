x <- rnorm(100)
y <- rnorm(100)
n <- length(x)

LinReg <- function(X, Y) {
  plot(X, Y)
  a <- CalcA(X, Y)
  b <- CalcB(X, Y)
  
  lines(X, a * X + b, type = "l", lty = 1, col = "red")
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
LinReg(x, y)
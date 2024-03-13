x <- c(1, 2, 3, 4)
y <- c(15, 33, 61, 99)
n <- length(x)
# y = 5x^2+3x+7
PolReg <- function(X, Y) {
  CalcA2(X, Y)
  CalcA0(X, Y)
}

CalcA2 <- function(X, Y) {
  n <- length(x)
  sumx2y <- Summary(X^2, Y^1)
  sumx3 <- Summary(X^3)
  sumx2 <- Summary(X^2)
  sumxy <- Summary(X^1, Y^1)
  sumx2 <- Summary(X^2)
  sumx <- Summary(X^1)
  sumy <- Summary(Y^1)

  sumx4 <- Summary(X^4)

  upper <- sumx2y * (sumx2 * n - sumx * sumx) - sumx3 * (sumxy * n - sumy * sumx) + sumx2 * (sumxy * sumx - sumy * sumx2)
  bottom <- sumx4 * (sumx2 * n - sumx * sumx) - sumx3 * (sumx3 * n - sumx2 * sumx) + sumx2 * (sumx3 * sumx - sumx2 * sumx2)

  A2 <- upper / bottom
  print(A2)
}

CalcA0 <- function(X, Y) {
  n <- length(x)
  sumx4 <- Summary(X^4)
  sumx3 <- Summary(X^4)
  sumx2y <- Summary(X^2, Y)
  sumx3 <- Summary(X^3)
  sumx2 <- Summary(X^2)
  sumxy <- Summary(X,Y)
  sumx <- Summary(X)
  sumy <- Summary(Y)

  upper <- sumx4 * (sumx2 * sumy - sumx * sumxy) - sumx3 * (sumx3 * sumy - sumx2 * sumxy) + sumx2y * (sumx3 * sumx - sumx2 * sumx2)
  bottom <- sumx4 * (sumx2 * n - sumx * sumx) - sumx3 * (sumx3 * n - sumx2 * sumx) + sumx2 * (sumx3 * sumx - sumx2 * sumx2)

  A0 <- upper / bottom
  print(A0)
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
PolReg(x, y)

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
y <- c(15, 33, 61, 99, 147, 205, 273, 351, 439, 537, 645, 763, 891, 1029, 1177, 1347, 1523, 1717, 1927, 2067)
n <- length(x)
# y = 5x^2+3x+7
PolReg <- function(X, Y) {
  plot(X, Y)
  sumx = Summary(X)
  sumx2 = Summary(X^2)
  A2 <- CalcA2(X, Y)
  A1 <- CalcA1(X, Y)
  A0 <- CalcA0(X, Y)
  #curve(A2 * X^2 + A1 * X + A0)
  lines(X, A2 * X^2 + A1 * X + A0, type = "l", lty = 1, col = "red")
}

CalcA2 <- function(X, Y) {
  n <- length(X)
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

CalcA1 <- function(X, Y) {
  sumx4 <- Summary(X^4)
  sumx2y <- Summary(X^2, Y)
  sumx2 <- Summary(X^2)
  sumx3 <- Summary(X^3)
  sumxy <- Summary(X, Y)
  sumx <- Summary(X)

  sumy <- Summary(Y)

  n <- length(X)



  upper <- sumx4 * (sumxy * n - sumy * sumx) - sumx2y * (sumx3 * n - sumx2 * sumx) + sumx2 * (sumx3 * sumy - sumx2 * sumxy)
  bottom <- sumx4 * (sumx2 * n - sumx * sumx) - sumx3 * (sumx3 * n - sumx2 * sumx) + sumx2 * (sumx3 * sumx - sumx2 * sumx2)

  A1 <- upper / bottom
  print(A1)
}

CalcA0 <- function(X, Y) {
  n <- length(x)
  sumx4 <- Summary(X^4)
  sumx3 <- Summary(X^4)
  sumx2y <- Summary(X^2, Y)
  sumx3 <- Summary(X^3)
  sumx2 <- Summary(X^2)
  sumxy <- Summary(X, Y)
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

RollDice <- function(N, K, P) {
    n <- length(K)
    sum <- 0
    for (i in 1:n) {
        sum <- sum + ((K[i] - (N * P))^2) / (600 * P)
    }
    print(sum)
}


Summary <- function(X, Y) {
    sum <- 0

    if (missing(Y) || Y == 0) {
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



dice <- c(1, 2, 3, 4, 5, 6)
k <- c(81,91,122,107,74,123)
p <- 1/6

print(RollDice(600, k, p))

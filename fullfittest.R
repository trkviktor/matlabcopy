RollDice <- function(N, K, P) {
    sum <- 0
    for (i in 1:n) {
        sum <- sum + ((K[i] - (N * P[i]))^2) / (600 * P[i])
    }
    
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
k <- c(1)
p <- c(1)

print(RollDice(dice, k, n))

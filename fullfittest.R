RollDice <- function(N, K, P) {
    n <- length(K)
    sum <- 0
    for (i in 1:n) {
        sum <- sum + ((K[i] - (N * P))^2) / (600 * P)
    }
    print(sum)
}

k <- c(81,91,122,107,74,123)
p <- 1/6

print(RollDice(600, k, p))

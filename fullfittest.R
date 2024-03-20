RollDice <- function(N, K, P) {
    n <- length(K)
    sum <- 0
    for (i in 1:n) {
        sum <- sum + ((K[i] - (N * P[i]))^2) / (600 * P[i])
    }

    #ez nem tudom kell-e mjozsef2000 githubján fentvan de órán nem lett elmondva
    cat(sum, "<", qchisq(p = .90, df = n - 1), "?\n")
    cat("\n")
    if (sum < qchisq(p = .90, df = n - 1)) {
        cat("Az erteket elfogadjuk!\n")
    } else {
        cat("Az erteket nem fogadjuk el!\n")
    }
}

k <- c(83, 91, 122, 107, 74, 123)
p <- c(1 / 6, 1 / 6, 1 / 6, 1 / 6, 1 / 6, 1 / 6)

RollDice(600, k, p)

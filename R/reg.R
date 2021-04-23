reg <- function(df, j) {
    if (j == 1) {
        lr <- lm(data = df, d ~ z1)
    } else {
        lr <- lm(data = df, d ~ z2)
    }
    return(lr)
}

varcov <- function(lr) {
    return(sandwich::vcovHC(lr, type = "HC1"))
}

sysavar <- function(lr) {
    stopifnot(length(lr) == 2)
    uhat1 <- residuals(lr[[1]])
    uhat2 <- residuals(lr[[2]])
    x1 <- model.matrix(lr[[1]])
    x2 <- model.matrix(lr[[2]])
    A <- magic::adiag(solve(t(x1) %*% x1), solve(t(x2) %*% x2))

    Xhat1 <- x1 * uhat1 # will multiply each element of each row by uhat1[i]
    Xhat2 <- x2 * uhat2
    topblock <- cbind((t(Xhat1) %*% Xhat1), (t(Xhat1) %*% Xhat2))
    bottomblock <- cbind((t(Xhat2) %*% Xhat1), (t(Xhat2) %*% Xhat2))
    B <- rbind(topblock, bottomblock)

    avar <- nrow(x1) * (A %*% B %*% A)
    avar <- avar[c("z1", "z2"), c("z1", "z2")]
    return(avar)
}

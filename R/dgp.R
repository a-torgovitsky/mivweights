dgp <- function(nu1 = 0, N = 200, seed = 1,
                z2.prob = .5,
                g.prob = c("at" = 2/12,
                           "ec" = 1/12,
                           "rc" = 1/12,
                           "nt" = 2/12,
                           "1c" = 5/12,
                           "2c" = 1/12)) {

    stopifnot(g.prob["1c"] > g.prob["2c"]) # only coded for this case
    set.seed(seed)

    g <- e1071::rdiscrete(N, unname(g.prob), names(g.prob))
    z2 <- rbinom(N, 1, z2.prob)

    # compute nu0 implied by the group probabilities
    nu0 <- qnorm(
             (z2.prob * (g.prob["rc"] + g.prob["1c"]) +
                g.prob["ec"] + g.prob["2c"]) /
             (g.prob["ec"] + g.prob["1c"])
    )

    # draw z1 conditional on z2
    z1 <- rep(NA, N)
    z20 <- (z2 == 0)
    z1[z20] <- rbinom(sum(z20), 1, pnorm(nu0))
    z21 <- (z2 == 1)
    z1[z21] <- rbinom(sum(z21), 1, pnorm(nu1))
    stopifnot(!all(is.na(z1)))

    # determine treatment based on groups and z
    d <- rep(0, N) # never-taker by default
    d[g == "at"] <- 1
    d[(g == "ec") & (z1 | z2)] <- 1
    d[(g == "rc") & (z1 & z2)] <- 1
    d[(g == "1c") & z1] <- 1
    d[(g == "2c") & z2] <- 1

    df <- data.frame(d = d, z1 = z1, z2 = z2, g = g)
    return(df)
}

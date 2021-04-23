# Bonferonni correction
bonf <- function(p) {
    # Bonferroni adjustment
    p <- sapply(p, function(x) min(2*x,1))

    # reject if either rejects after adjustment
    return(min(p))
}

# Intersection-union test (IUT)
iut <- function(p) {
    # reject if BOTH reject
    return(max(p))
}

# Min test
mintest <- function(lr, ndraws = 10000, pgridstep = .0001, seed = 1) {
    stopifnot(length(lr) == 2)
    avar <- sysavar(lr)
    astd <- sqrt(diag(avar))

    N1 <- nobs(lr[[1]])
    N2 <- nobs(lr[[2]])
    stopifnot(N1 == N2)
    t1 <- sqrt(N1) * (lr[[1]]$coeff["z1"] / astd[1])
    t2 <- sqrt(N1) * (lr[[2]]$coeff["z2"] / astd[2])

    tmin <- min(t1, t2) # test statistic

    # Asymptotic variance of the t statistics
    acorr <- avar[1,2] / (astd[1] * astd[2])
    tstat_avar <- matrix(c(1, acorr, acorr, 1), nrow = 2)

    set.seed(seed)
    draws <- MASS::mvrnorm(n = ndraws, mu = c(0,0), Sigma = tstat_avar)
    mindraws <- matrixStats::rowMins(draws)
    alphalist = seq(from = pgridstep, to = 1, by = pgridstep)
    cv <- quantile(mindraws, probs = alphalist, names = FALSE)

    idx <- Position(function(x) tmin < x, cv)
    if (is.na(idx)) { # tmin was never smaller than anything in cv?
        pval <- 1
    } else {
        pval <- alphalist[idx]
    }
    return(pval)
}

# Cox and Shi (2019) QLR test
coxshi <- function(lr, tol = 1e-6) {
    avar <- sysavar(lr)
    theta <- c(lr[[1]]$coeff["z1"], lr[[2]]$coeff["z2"])

    # solve the problem:
    # min_{t <= 0} (-\theta - t)' \Sigma^{-1}(-\theta - t)
    #
    # objective function is equal to
    # (\theta + t)'\Sigma^{-1}(\theta + t)
    #
    # expands out to:
    # t'\Sigma^{-1}t + 2t'\Sigma^{-1}\theta + \theta'\theta
    model <- list()

    avarinv <- solve(avar)
    model$Q <- avarinv
    model$obj <- 2 * avarinv %*% theta
    model$objcon <- t(theta) %*% avarinv %*% theta
    model$lb <- c(-Inf, -Inf)
    model$ub <- c(0, 0)
    model$modelsense <- "min"
    model$A <- matrix(c(0,0), ncol = 2) # no default
    model$sense <- "=" # default would be 0 < 0, so make it =

    result <- gurobi::gurobi(model, list(OutputFlag = 0))
    if (result$status != "OPTIMAL") {
        warning("Cox-Shi solve failed?")
    }

    # test statistic
    Tn <- nobs(lr[[1]]) * result$objval

    # determine number of degrees of freedom based on how many components
    # of x are binding (equal to 0)
    degfr <- sum(result$x > (-1 * tol))

    # Note that pchisq(something small, 0) is 1 not 0
    # That's a problem when solving numerically, since we never get Tn as an
    # exact zero. Analytically this wouldn't be a problem.
    if (Tn < tol) {
        pval <- 1
    } else {
        pval <- 1 - pchisq(Tn, degfr)
    }

    return(pval)
}

# Romano, Shaikh and Wolf (2014) test
rswtest <- function (lr, numbs = 2000, betafrac = .1, pgridstep = .001) {
    avar <- sysavar(lr)
    astd <- diag(avar)
    theta <- c(lr[[1]]$coeff["z1"], lr[[2]]$coeff["z2"])
    mu <- -1*theta # to put it in the language of RSW
    stopifnot(nobs(lr[[1]]) == nobs(lr[[2]]))
    N <- nobs(lr[[1]])

    # Null is mu[1] <= 0, mu[2] <= 0, so test stat is the max
    t1 <- sqrt(N) * (mu[1] / astd[1])
    t2 <- sqrt(N) * (mu[2] / astd[2])
    ts <- max(t1, t2)

    # Bootstrap mu
    mubs <- matrix(NA, nrow = numbs, ncol = 2)
    for (b in seq_len(numbs)) {
        idx <- sample.int(N, replace = TRUE)

        for (j in seq_len(2)) {
            lrr <- lm(data = lr[[j]]$model[idx, ],
                      lr[[j]])
            # remember to multiply by -1
            mubs[b,j] <- -1 * lrr$coeff[paste0("z", j)]
        }
    }

    # center and scale the bootstrapped estimates
    mubs_cs <- sweep(mubs, 2, mu, FUN = "-")
    mubs_cs <- sweep(mubs_cs, 2, astd, FUN = "/")
    mubs_cs <- sweep(mubs_cs, 2, sqrt(N), FUN = "*")
    mubsmax <- matrixStats::rowMaxs(mubs_cs)

    alphalist = seq(from = pgridstep, to = 1, by = pgridstep)
    betalist <- alphalist*betafrac
    rej <- rep(NA, length(alphalist))

    # first step quantile
    fsq <- quantile(mubsmax, 1 - betalist, names = FALSE)

    for (i in seq_along(betalist)) {
        r <- c(mu[1] + astd[1]*fsq[i]/sqrt(N),
               mu[2] + astd[2]*fsq[i]/sqrt(N))
        lambdastar <- c(min(r[1], 0), min(r[2], 0))

        if (all(r <= 0)) {
            rej[i] <- 0
        } else {
            # add mu + lambdastar to each bootstrap draw and scale
            mubs_cs <- sweep(-1 * mubs, 2, mu + lambdastar, FUN = "+")
            mubs_cs <- sweep(mubs_cs, 2, astd, FUN = "/")
            mubs_cs <- sweep(mubs_cs, 2, sqrt(N), FUN = "*")
            mubsmax <- matrixStats::rowMaxs(mubs_cs)

            cv <- quantile(mubsmax, 1 - alphalist[i] + betalist[i],
                           names = FALSE)
            rej[i] = (ts > cv)
        }
    }

    idx <- Position(function(x) x, rej)
    if (is.na(idx)) {
        pval <- 1
    } else {
        pval <- alphalist[idx]
    }
    return(pval)
}

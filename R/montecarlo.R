runmcs <- function(nu1, samplesize, nreps = 100) {
    mcparams <- expand_grid(nu1, samplesize)
    tblist <- vector(mode = "list", length = nrow(mcparams))
    resultslist <- vector(mode = "list", length = nrow(mcparams))
    for (i in seq_len(nrow(mcparams))) {
        output <- paste0("Starting simulation ", i, "/", nrow(mcparams), ".")
        message(output)
        mcp <- mcparams[i,]
        output <- paste0("\t nu1 = ", mcp$nu1,
                         "; samplesize = ", mcp$samplesize)
        message(output)

        resultslist[[i]] <- montecarlo(nreps = nreps,
                                       nu1 = mcp$nu1,
                                       samplesize = mcp$samplesize)
        resultslist[[i]]$nu1 <- rep(mcp$nu1, nrow(resultslist[[i]]))
        resultslist[[i]]$samplesize <- rep(mcp$samplesize,
                                           nrow(resultslist[[i]]))
        tblist[[i]] <- rejprob(resultslist[[i]])
        tblist[[i]]$nu1 <- rep(mcp$nu1, nrow(tblist[[i]]))
        tblist[[i]]$samplesize <- rep(mcp$samplesize,
                                      nrow(tblist[[i]]))
    }
    tbp <- bind_rows(tblist)
    rawresults <- bind_rows(resultslist)

    return(list(rej = tbp, rawresults = rawresults))
}

montecarlo <- function(nreps = 100, samplesize = 500, nu1 = 0) {
    rl <- vector(mode = "list", length = nreps)
    rl <- future.apply::future_lapply(seq_len(nreps), function (m) {
        df <- dgp(nu1 = nu1, N = samplesize, seed = m)
        lr <- list(reg(df, 1), reg(df, 2))

        theta <- rep(NA, 2)
        se <- rep(NA, 2)
        tstat <- rep(NA, 2)
        ppos <- rep(NA, 2)
        pneg <- rep(NA, 2)
        for (j in seq_len(2)) {
            # point estimates, standard errors, t--values
            zname <- paste0("z", j)
            theta[j] <- lr[[j]]$coeff[zname]
            vc <- varcov(lr[[j]])
            se[j] <- sqrt(vc[zname, zname])
            tstat[j] <- theta[j] / se[j]

            # p-values for one-sided tests
            #   pos is for the null that theta_{j} >= 0
            #   neg is for the null that theta_{j} <= 0
            ppos[j] <- pnorm(tstat[j])
            pneg[j] <- 1 - pnorm(tstat[j])
        }
        # Tests of
        # H_{0}: theta_{1} >= 0 and theta_{2} >= 0
        pbonf <- bonf(ppos)
        pmintest <- mintest(lr)
        pcoxshi <- coxshi(lr)
        prsw <- rswtest(lr)

        # Test of
        # H_{0}: theta_{1} <= 0 OR theta_{2} <= 0
        piut <- iut(pneg)

        # Just to check my variance calculation
        avar <- sysavar(lr)
        fvar <- avar / samplesize
        mysd1 <- sqrt(fvar[1,1])
        mysd2 <- sqrt(fvar[2,2])
        mycor <- fvar[1,2]/sqrt(fvar[1,1] * fvar[2,2])

        r <- list(theta1 = theta[1], theta2 = theta[2],
                  se1 = se[1], se2 = se[2],
                  t1 = tstat[1], t2 = tstat[2],
                  ppos1 = ppos[1], ppos2 = ppos[2],
                  pneg1 = pneg[1], pneg2 = pneg[2],
                  pbonf = pbonf, pmintest = pmintest, pcoxshi = pcoxshi,
                  prsw = prsw, piut = piut,
                  mysd1 = mysd1, mysd2 = mysd2, mycor = mycor)
        return(r)
    }, future.seed = NULL)
    results <- bind_rows(rl)
    return(results)
}

rejprob <- function(r) {
    tbp <- tibble(
        alpha = seq(from = .01, to = .5, by = .005)
    )
    tbp$Bonferroni <- computepvals(tbp$alpha, r$pbonf)
    tbp$IUT <- computepvals(tbp$alpha, r$piut)
    tbp$Mintest <- computepvals(tbp$alpha, r$pmintest)
    tbp$"Cox-Shi" <- computepvals(tbp$alpha, r$pcoxshi)
    tbp$RSW <- computepvals(tbp$alpha, r$prsw)

    tbp <- gather(tbp, "Bonferroni", "Cox-Shi", "Mintest", "RSW", "IUT",
                  key = "Test", value = "rejprob")
    return(tbp)
}

computepvals <- function(alphalist, column) {
    sapply(alphalist, function (a) mean(column <= a))
}

NREPS_DEFAULT <- 2000

figure1 <- function(savedir = ".", nreps = NREPS_DEFAULT) {
    results <- runmcs(nu1 = c(-.25, 0, .25),
                      samplesize = c(200, 2000),
                      nreps = nreps)
    save_results(results, suffix = "figure1")
    plot_figure1(results$rej)
    return(results)
}

figure2 <- function(savedir = ".", nreps = NREPS_DEFAULT) {
    results <- runmcs(nu1 = seq(from = -.4, to = .4, by = .02),
                      samplesize = c(1000),
                      nreps = nreps)
    save_results(results, suffix = "figure2")
    plot_figure2(results$rej)
    return(results)
}

save_results <- function(results, suffix = "") {
    write_csv(results$rawresults, paste0("rawresults-", suffix, ".csv"))
    write_csv(results$rej, paste0("rejprob-", suffix, ".csv"))
}

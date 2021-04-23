plot_figure1 <- function(rej) {
    rej <- rej %>%
        filter(alpha <= .2) %>%
        arrange(samplesize, nu1) # gets facets in the right order

    nu1label <- function(s) paste0("$\\nu_{1} = ", s, "$")
    samplesizelabel <- function(s) paste0("$n = ", s, "$")

    p <- ggplot(data = rej) +
        geom_line(aes(x = alpha, y = rejprob, color = Test)) +
        geom_line(aes(x = alpha, y = alpha), linetype = "dotted") +
        labs(
             x = "Nominal level",
             y = "Rejection probability"
        ) +
        facet_grid(nu1 ~ samplesize,
                   labeller = labeller(nu1 = nu1label,
                                       samplesize = samplesizelabel),
                   scales = "free_y")
    printtikz(p, "figure1.tex")
}

plot_figure2 <- function(rej, level = .05,
                         arrowoffset = .025, arrowheight = .7,
                         arrowlength = .2) {
    rej <- rej %>%
        filter(alpha == level)
    samplesize <- unique(rej$samplesize)

    p <- ggplot(data = rej) +
        geom_line(aes(x = nu1, y = rejprob, color =  Test)) +
        geom_hline(yintercept = level, linetype = "dotted") +
        geom_vline(xintercept = 0, linetype = "dotted") +
        labs(
             x = "$\\nu_{1}$",
             y = "Rejection probability"
        ) +
        annotate("segment",
             x = -1 * arrowoffset, xend = -1 * arrowlength,
             y = arrowheight, yend = arrowheight,
             lineend = "round", linejoin = "bevel",
             arrow = arrow(type = "closed",
                           angle = 20,
                           length = unit(3, "mm"))) +
        annotate("segment",
             x = arrowoffset, xend = arrowlength,
             y = arrowheight, yend = arrowheight,
             lineend = "round", linejoin = "bevel",
             arrow = arrow(type = "closed",
                           angle = 20,
                           length = unit(3, "mm"))) +
        annotate("text",
                 x = (arrowlength + arrowoffset) / 2,
                 y = arrowheight + arrowoffset,
                 label = "$H_{0}^{+}$") +
        annotate("text",
                 x = (-1 * arrowlength - arrowoffset) / 2,
                 y = arrowheight + arrowoffset,
                 label = "$H_{0}^{-}$")
    printtikz(p, "figure2.tex")
}

printtikz <- function(p, fn) {
    tikzDevice::tikz(file = fn, standAlone = TRUE)
    print(p)
    dev.off()
    tools::texi2pdf(fn, clean = TRUE)
}

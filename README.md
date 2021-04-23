# mivweights

This package contains the code for the Monte Carlo simulations in "The Causal Interpretation of Two-Stage Least Squares with Multiple Instrumental Variables" by Mogstad, Torgovitsky, and Walters.

Install the latest version of the package with:
```r
devtools::install_github("a-torgovitsky/mivweights")
```

Figures 1 and 2 in the supplemental appendix can be replicated as follows:
```r
library("mivweights")

# Uncomment the following two lines and adjust workers if you want to run in parallel
# library("future")
# plan(multicore, workers = 35)

figure1()
figure2()
```

# mivweights

This package contains the code for the Monte Carlo simulations in "The Causal Interpretation of Two-Stage Least Squares with Multiple Instrumental Variables" by Mogstad, Torgovitsky, and Walters.

The package is meant to be in source state, not built or installed.
Obtain the files (e.g. clone this repo) then load the package with:
```r
devtools::load_all()
```

Figures 1 and 2 in the supplemental appendix can then be replicated as follows:
```r
# Uncomment the following two lines and adjust workers if you want to run in parallel
# library("future")
# plan(multicore, workers = 35)

figure1()
figure2()
```
This will produce some csv files with data, some tex files for the figures, and some pdf's with compiled figures.

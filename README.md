# mtw2020mc
Repo: [https://github.com/a-torgovitsky/mtw2020mc](https://github.com/a-torgovitsky/mtw2020mc)

This package contains the code for the Monte Carlo simulations in "The Causal Interpretation of Two-Stage Least Squares with Multiple Instrumental Variables" by Mogstad, Torgovitsky, and Walters.

## Initial prerequisites
- Gurobi and the Gurobi R package **gurobi**, which can be obtained from [Gurobi Optimization](http://www.gurobi.com/index). A Gurobi software license is available at no cost to academic researchers. A clear installation guide for Gurobi can be found [here](https://cran.r-project.org/package=prioritizr/vignettes/gurobi_installation.html).
- A LaTeX distribution, such as [TeX live](https://www.tug.org/texlive/). (This is only required to convert figures made in `ggplot2` to Tikz. The code in `plots.R` can be easily adjusted to save the native `ggplot2` figures by simply ignoring the function `printtikz`.)

## Install dependencies

First make sure that the `gurobi` package is installed and install `devtools` if it isn't already installed:
```r
install.packages("devtools")
```
Obtain the source files (e.g. clone the repo), then install dependencies from CRAN:
```r
devtools::install_deps(pkg = ".", dependencies = TRUE)
```

## Usage

Figures 1 and 2 in the supplemental appendix can then be replicated as follows:
```r
devtools::load_all()

# Uncomment the following two lines and adjust workers if you want to run in parallel
# library("future")
# plan(multicore, workers = 35)

figure1()
figure2()
```
This will produce some csv files with data, some tex files for the figures, and some pdf's with compiled figures.

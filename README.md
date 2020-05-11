
# strand: A framework for investment strategy simulation

[![Build
Status](https://travis-ci.org/strand-tech/strand.svg?branch=master)](https://travis-ci.org/strand-tech/strand)
[![codecov](https://codecov.io/gh/strand-tech/strand/branch/master/graph/badge.svg)](https://codecov.io/gh/strand-tech/strand)

## Overview

`strand` provides a framework for performing discrete (share-level)
simulations of investment strategies. Simulated portfolios optimize
exposure to an input signal subject to constraints such as position size
and factor exposure.

The package vignette provides an in-depth discussion of setup and usage.
See `vignette("strand")`.

## Installation

``` r
# Install the latest version from CRAN:
install.packages("strand")

# Install development version from GitHub using devtools:
devtools::install_github("strand-tech/strand")
```

## Usage

Four ingredients are required to run a simulation using `strand`:

1.  *Configuration file*. A file in yaml format that describes the
    parameters of the simulation, such as the input signal, risk
    constraints, trading limits, position limits, the location of data
    inputs, etc.

2.  *Security reference*. A listing of all securities allowed in the
    simulation and any categorical values (such as sector and industry)
    that can be used in exposure constraints.

3.  *Signal, factor, and supplementary data*. Data for each day
    including the input signal (to which exposure is maximized) and any
    factors that appear in constraints. Supplementary data could
    include, for example, a daily measure of market capitalization for
    use in universe construction.

4.  *Pricing data*. Daily prices, dividends, and trading volume for
    computing market values and filling orders. Unadjusted prices and
    accompanying adjustment ratios may be used.

Here is how we would run the simulation discussed in the package
vignette:

1.  Clone the repository and extract the sample data.

<!-- end list -->

``` console
$ git clone git@github.com:strand-tech/strand.git
$ cd strand
$ tar zxf sample_data.tar.gz
```

2.  With the same working directory in R, create a simulation object
    configured using the vignette example’s yaml file:

<!-- end list -->

``` r
library(strand)
sim <- Simulation$new("vignettes/sample.yaml")
```

3.  Run and summarize:

<!-- end list -->

``` r
sim$run()
sim$overallStatsDf()
```

    ##                            Item  Gross       Net
    ## 1                     Total P&L -8,773   -35,912
    ## 2       Total Return on GMV (%)   -0.4      -1.9
    ## 3  Annualized Return on GMV (%)   -0.4      -1.8
    ## 4            Annualized Vol (%)    0.6       0.6
    ## 5             Annualized Sharpe  -0.71     -2.97
    ## 6                       Avg GMV        1,992,131
    ## 7                       Avg NMV              -94
    ## 8                     Avg Count              387
    ## 9            Avg Daily Turnover           27,092
    ## 10      Holding Period (months)              7.0

### Docker demo

If you have `docker` and `docker-compose` installed, you can run a
sample strand shiny application by cloning the repository and running
the following commands from the top-level directory:

``` console
$ docker-compose build
$ docker-compose up
```

The application will run by default on port 80. To configure edit
`docker-compose.yml`.

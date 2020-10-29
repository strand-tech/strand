
# strand: A framework for investment strategy simulation

[![CRAN
status](https://www.r-pkg.org/badges/version/strand)](https://cran.r-project.org/package=strand)
[![Build
Status](https://travis-ci.org/strand-tech/strand.svg?branch=master)](https://travis-ci.org/strand-tech/strand)
[![codecov](https://codecov.io/gh/strand-tech/strand/branch/master/graph/badge.svg)](https://codecov.io/gh/strand-tech/strand)

## Overview

`strand` provides a framework for performing discrete (share-level)
simulations of investment strategies. Simulated portfolios optimize
exposure to an input signal subject to constraints such as position size
and factor exposure.

The [package
vignette](https://cran.r-project.org/package=strand/vignettes/strand.html)
provides an in-depth discussion of setup and usage. See
`vignette("strand")`.

## Features

  - Straightforward data interfaces.
  - Option to load daily data from binary (feather) files for fast
    access and low memory footprint.
  - Share-level bookkeeping.
  - YAML-based configuration.
  - Position sizing based on portfolio weight and percentage of expected
    volume.
  - Trade sizing based on percentage of expected volume.
  - Ability to specify constraints on factor exposure, category
    exposure, and turnover.
  - Automatic loosening of factor and category exposure constraints if
    no solution is found.
  - Realistic trade filling based on percentage of actual volume.

### Tiingo and EDGAR sample data

The package’s sample data set includes value and size factors, as well
as daily pricing information, for most of the stocks in the S\&P 500 for
the period June-August 2020. The source of fundamental data is
[EDGAR](https://www.sec.gov/edgar.shtml), and all pricing data was
downloaded using the [Tiingo Stock API](https://api.tiingo.com/).
Special thanks to Tiingo for giving permission to release this data.

## Installation

``` r
# Install the latest version from CRAN:
install.packages("strand")

# Install development version from GitHub using remotes:
install.packages("remotes")
remotes::install_github("strand-tech/strand")
```

### Note on solvers

The `strand` package uses [GLPK](https://www.gnu.org/software/glpk/) as
the default solver for portfolio optimization. As a result, it depends
on package `Rglpk`.

It is possible to use [SYMPHONY](https://github.com/coin-or/SYMPHONY)
instead, by setting `solver: symphony` in the simulation’s configuration
file and installing the `Rsymphony` package. Note that you will need to
install SYMPHONY on your system first, and [on OS X perform a few extra
steps](https://cran.r-project.org/package=TestDesign/vignettes/rsymphony.html)
to install `Rsymphony`.

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

<!-- end list -->

``` r
library(strand)

# Load up sample data
data(sample_secref)
data(sample_pricing)
data(sample_inputs)

# Load sample configuration file
config <- example_strategy_config()

# Create the Simulation object and run
sim <- Simulation$new(config,
                      raw_input_data = sample_inputs,
                      raw_pricing_data = sample_pricing,
                      security_reference_data = sample_secref)
sim$run()

# Print overall statistics
sim$overallStatsDf()
```

    ##                            Item   Gross       Net
    ## 1                     Total P&L -32,355   -40,867
    ## 2       Total Return on GMV (%)    -1.8      -2.3
    ## 3  Annualized Return on GMV (%)    -6.9      -8.7
    ## 4            Annualized Vol (%)     8.9       8.8
    ## 5             Annualized Sharpe   -0.77     -0.98
    ## 6              Max Drawdown (%)    -6.6      -6.8
    ## 7                       Avg GMV         1,976,540
    ## 8                       Avg NMV              -716
    ## 9                     Avg Count               189
    ## 10           Avg Daily Turnover            53,022
    ## 11      Holding Period (months)               3.6

### Example shiny application (local)

To run an example shiny application that allows interactively
configuring and running a simulation:

``` r
library(strand)
example_shiny_app()
```

### Example shiny application (docker)

If you have `docker` and `docker-compose` installed, you can run the
example shiny application by cloning the [github
repository](https://github.com/strand-tech/strand) and running the
following commands from the top-level directory:

``` console
$ docker-compose build
$ docker-compose up
```

The application will run by default on port 80. To configure edit
`docker-compose.yml`.

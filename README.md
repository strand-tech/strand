# strand: A framework for investment strategy simulation

[![Build Status](https://travis-ci.org/strand-tech/strand.svg?branch=master)](https://travis-ci.org/strand-tech/strand)
[![codecov](https://codecov.io/gh/strand-tech/strand/branch/master/graph/badge.svg)](https://codecov.io/gh/strand-tech/strand)

## Overview

The `strand` package provides a framework for performing discrete (share-level) simulations of investment strategies. Simulated portfolios optimize exposure to an input signal subject to constraints such as position size and factor exposure.

## Installation

``` r
# Install the latest version from CRAN:
install.packages("strand")

# Install development version from GitHub using devtools:
devtools::install_github("strand-tech/strand")
```

### Docker demo

If you have `docker` and `docker-compose` installed, you can run a sample strand shiny application by cloning the repository and running the following commands from the top-level directory:

```
$ docker-compose build
$ docker-compose up
```

The application will run by default on port 80. To configure edit `docker-compose.yml`.



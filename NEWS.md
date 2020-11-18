# strand 0.2.0

* The sample dataset has been replaced with real-world data. Pricing data has
been sourced from the Tiingo Stock API, and financial data has been sourced from
EDGAR. The dataset includes size and value factors for most of the stocks in the
S&P 500 for the period June-August 2020.

* The sample shiny app has multiple improvements, including the ability to load
simulation results and improved visualizations.

* The `PortOpt` class has been exported. This class can be used to solve single
portfolio optimization problems.

* Writing html reports for a simulation based on an Rmd template is now possible
using the `writeReport` method of the `Simulation` class.

* Delistings can now be entered with a delisting return.

* Average volume has been changed to a market value measure from a shares
measure. This makes it much easier to provide a measure of ADV (average daily
volume) in a reference currency for setting trading and position limit
constraints. By default the column is called `rc_vol`.

* Added input_dates parameter to Simulation class constructor. The date vector
passed to input_dates contols the dates on which input data (e.g., alpha and
risk factors) is updated.

* Added force-trim feature to trim back positions that grow too large. Setting
the simulator configuration parameter `force_trim_factor` to a numeric value X
will cause orders to be created that trim positions back to X times their
maximum allowable size.

* Added force-exit feature for non-investable securities. Setting the simulator
configuration parameter `force_exit_non_investable` to `TRUE` will cause exit
orders to be created for positions not in the universe during a simulation.

* Added feature to limit LMV/SMV increase/decrease on a single day. This feature
is useful when ramping up the portfolio to its target LMV/SMV at the beginning
of the simulation. For example, setting the simulator configuration parameter
`max_weight_change` to 0.1 will limit LMV/SMV increase to 10% of the target
LMV/SMV per day.

* Now saving gross, long, and short exposures in addition to net.

* Standardized simulation result data and plotting methods to have a
strategy_name parameter where appropriate.

* Improved handling of delistings.

* Added new plotting methods for the `Simulation` class: `plotContribution`,
`plotTurnover`, `plotUniverseSize`, and `plotNonInvestablePct`.

* Added `overallReturnsByMonthDf` method for the `Simulation` class.

* Fixed recording of loosened constraints during simulation.

* Fixed loading of configuration information in `readFeather`.

# strand 0.1.3 (2020-05-24)

* The example in the package vignette now uses the sample datasets included in
the package.

* `overallStatsDf()` includes a max drawdown entry.

# strand 0.1.2 (2020-05-22)

* `Rsymphony` dependency has been moved from Imports to Suggests to make
installation easier.

* Example shiny application now uses the sample datasets provided with the package,
and can now be run directly by calling the new function `example_shiny_app()`.

* New function `example_strategy_config()` provides an easy way to grab the
sample configuration used in examples and the shiny app.

# strand 0.1.1 (2020-05-18)

* Initial version on CRAN
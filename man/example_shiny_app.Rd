% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/examples.R
\name{example_shiny_app}
\alias{example_shiny_app}
\title{Run an example shiny app}
\usage{
example_shiny_app()
}
\description{
Runs a shiny app that allows interactively configuring and running a
simulation. Once the simulation is finished results, such as performance
statistics and plots of exposures, are available in a results panel.
}
\examples{

if (interactive()) {
  example_shiny_app()
}

}

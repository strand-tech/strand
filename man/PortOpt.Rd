% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PortOpt.R
\name{PortOpt}
\alias{PortOpt}
\title{Portfolio optimization class}
\description{
The \code{PortOpt} object is used to set up and solve a
  portfolio optimization problem.
}
\details{
A \code{PortOpt} object is configured in the same way as a
  \code{Simulation} object, by supplying configuration in a yaml file or list
  to the object constructor. Methods are available for adding constraints and
  retrieving information about the optimization setup and results. See the
  package vignette for information on configuration file setup.
}
\examples{

## ------------------------------------------------
## Method `PortOpt$new`
## ------------------------------------------------

library(dplyr)
data(sample_secref)
data(sample_inputs)
data(sample_pricing)

# Construct optimization input for one day from sample data. The columns
# of the input data must match the input configuration.
optim_input <-
  inner_join(sample_inputs, sample_pricing,
             by = c("id", "date")) \%>\%
  left_join(sample_secref, by = "id") \%>\%
  filter(date \%in\% as.Date("2020-06-01")) \%>\%
  mutate(ref_price = price_unadj,
                shares_strategy_1 = 0)

opt <-
  PortOpt$new(config = example_strategy_config(),
              input_data = optim_input)

# The problem is not solved until the \code{solve} method is called
# explicitly.
opt$solve()
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{PortOpt$new()}}
\item \href{#method-setVerbose}{\code{PortOpt$setVerbose()}}
\item \href{#method-addConstraints}{\code{PortOpt$addConstraints()}}
\item \href{#method-getConstraintMatrix}{\code{PortOpt$getConstraintMatrix()}}
\item \href{#method-getConstraintMeta}{\code{PortOpt$getConstraintMeta()}}
\item \href{#method-solve}{\code{PortOpt$solve()}}
\item \href{#method-getResultData}{\code{PortOpt$getResultData()}}
\item \href{#method-getLoosenedConstraints}{\code{PortOpt$getLoosenedConstraints()}}
\item \href{#method-getMaxPosition}{\code{PortOpt$getMaxPosition()}}
\item \href{#method-getMaxOrder}{\code{PortOpt$getMaxOrder()}}
\item \href{#method-summaryDf}{\code{PortOpt$summaryDf()}}
\item \href{#method-print}{\code{PortOpt$print()}}
\item \href{#method-clone}{\code{PortOpt$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{PortOpt} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$new(config, input_data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{config}}{An object of class \code{list} or \code{character}. If the
value passed is a character vector, it should be of length 1 and
specify the path to a yaml configuration file that contains the
object's configuration info. If the value passed is of class list(),
the list should contain the object's configuration info in list form
(e.g, the return value of calling \code{yaml.load_file} on the
configuration file).}

\item{\code{input_data}}{A \code{data.frame} that contains all necessary input
for the optimization.

If the top-level configuration item \code{price_var} is not set, prices will be expected
in the \code{ref_price} column of \code{input_data}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{PortOpt} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{library(dplyr)
data(sample_secref)
data(sample_inputs)
data(sample_pricing)

# Construct optimization input for one day from sample data. The columns
# of the input data must match the input configuration.
optim_input <-
  inner_join(sample_inputs, sample_pricing,
             by = c("id", "date")) \%>\%
  left_join(sample_secref, by = "id") \%>\%
  filter(date \%in\% as.Date("2020-06-01")) \%>\%
  mutate(ref_price = price_unadj,
                shares_strategy_1 = 0)

opt <-
  PortOpt$new(config = example_strategy_config(),
              input_data = optim_input)

# The problem is not solved until the \code{solve} method is called
# explicitly.
opt$solve()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-setVerbose"></a>}}
\if{latex}{\out{\hypertarget{method-setVerbose}{}}}
\subsection{Method \code{setVerbose()}}{
Set the verbose flag to control the amount of informational
  output.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$setVerbose(verbose)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{verbose}}{Logical flag indicating whether to be verbose or not.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No return value, called for side effects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-addConstraints"></a>}}
\if{latex}{\out{\hypertarget{method-addConstraints}{}}}
\subsection{Method \code{addConstraints()}}{
Add optimization constraints.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$addConstraints(constraint_matrix, dir, rhs, name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{constraint_matrix}}{Matrix with one row per constraint and \eqn{(S+1) \times N}
  columns, where S is number of strategies and N is the number of stocks.

  The variables in the optimization are

  \deqn{x_{1,1}, x_{2,1}, \ldots, x_{N,1},}
  \deqn{x_{1,2}, x_{2,2}, \ldots, x_{N,2},}
  \deqn{\vdots}
  \deqn{x_{1,S}, x_{2,S}, \ldots, x_{N,S},}
  \deqn{y_1, \ldots, y_N}

  The first \eqn{N \times S} variables are the individual strategy
  trades. Variable \eqn{x_{i,s}} represents the signed trade for stock i
  in strategy s. The following N auxillary variables \eqn{y_1, \ldots, y_N}
  represent the absolute value of the net trade in each stock. So
  for a stock i, we have:

  \deqn{y_i = \sum_s |x_{i,s}|}}

\item{\code{dir}}{Vector of class character of length
\code{nrow(constraint_matrix)} that specifies the direction of the
constraints. All elements must be one of ">=", "==", or "<=".}

\item{\code{rhs}}{Vector of class numeric of length
\code{nrow(constraint_matrix)} that specifies the bounds of the
constraints.}

\item{\code{name}}{Character vector of length 1 that specifies a name for the
set of constraints that are being created.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
No return value, called for side effects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getConstraintMatrix"></a>}}
\if{latex}{\out{\hypertarget{method-getConstraintMatrix}{}}}
\subsection{Method \code{getConstraintMatrix()}}{
Constraint matrix access.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$getConstraintMatrix()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The optimization's constraint matrix.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getConstraintMeta"></a>}}
\if{latex}{\out{\hypertarget{method-getConstraintMeta}{}}}
\subsection{Method \code{getConstraintMeta()}}{
Provide high-level constraint information.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$getConstraintMeta()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A data frame that contains constraint metadata, such as current constraint value and
  whether a constraint is currently within bounds, for all single-row
  constraints. Explicitly exclude net trade constraints and constraints
  that involve net trade variables.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-solve"></a>}}
\if{latex}{\out{\hypertarget{method-solve}{}}}
\subsection{Method \code{solve()}}{
Solve the optimization. After running \code{solve()},
  results can be retrieved using \code{getResultData()}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$solve()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
No return value, called for side effects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getResultData"></a>}}
\if{latex}{\out{\hypertarget{method-getResultData}{}}}
\subsection{Method \code{getResultData()}}{
Get optimization result.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$getResultData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A data frame that contains the number of shares and the net
  market value of the trades at the strategy and joint (net) level
  for each stock in the optimization's input.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getLoosenedConstraints"></a>}}
\if{latex}{\out{\hypertarget{method-getLoosenedConstraints}{}}}
\subsection{Method \code{getLoosenedConstraints()}}{
Provide information about any constraints that were loosened
  in order to solve the optimization.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$getLoosenedConstraints()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Object of class \code{list} where keys are the names of the
  loosened constraints and values are how much they were loosened toward
  current values. Values are expressed as (current constraint value -
  loosened constraint value) / (current constraint value - violated
  constraint value). A value of 0 means a constraint was loosened 100\%
  and is not binding.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getMaxPosition"></a>}}
\if{latex}{\out{\hypertarget{method-getMaxPosition}{}}}
\subsection{Method \code{getMaxPosition()}}{
Provide information about the maximum position size allowed
  for long and short positions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$getMaxPosition()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
An object of class \code{data.frame} that contains the limits on
  size for long and short positions for each strategy and security. The
  columns in the data frame are:
  \describe{
    \item{id}{Security identifier.}
    \item{strategy}{Strategy name.}
    \item{max_pos_lmv}{Maximum net market value for a long position.}
    \item{max_pos_smv}{Maximum net market value for a short position.}
    }
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getMaxOrder"></a>}}
\if{latex}{\out{\hypertarget{method-getMaxOrder}{}}}
\subsection{Method \code{getMaxOrder()}}{
Provide information about the maximum order size allowed
  for each security and strategy.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$getMaxOrder()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
An object of class \code{data.frame} that contains the limit on
  order size for each strategy and security. The
  columns in the data frame are:
  \describe{
    \item{id}{Security identifier.}
    \item{strategy}{Strategy name.}
    \item{max_order_gmv}{Maximum gross market value allowed for an order.}
    }
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-summaryDf"></a>}}
\if{latex}{\out{\hypertarget{method-summaryDf}{}}}
\subsection{Method \code{summaryDf()}}{
Provide aggregate level optimization information if the
  problem has been solved.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$summaryDf()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A data frame with one row per strategy, including the joint (net)
  level, and columns for starting and ending market values and factor
  expoure values.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print summary information.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
No return value, called for side effects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PortOpt$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}

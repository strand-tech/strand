#' Sample security reference data for examples and testing
#'
#' A dataset containing sample reference data for the securities of 492 large
#' companies. All securities in the dataset were in the S&P 500 for most or all
#' of the period June-August 2020.
#'
#' @format A data frame with 492 rows and 4 variables:
#' \describe{
#'   \item{id}{Unique security identifier (the security's ticker).}
#'   \item{name}{Company name.}
#'   \item{symbol}{Human-readable symbol for display and reporting purposes. In
#'   the case of this dataset it is the same as the \code{id} variable.}
#'   \item{sector}{GICS sector for the company according to the Wikipedia page \href{https://en.wikipedia.org/wiki/List_of_S\%26P_500_companies}{List of S&P 500 companies}.} }
#' @usage data(sample_secref)
"sample_secref"

#' Sample security inputs for examples and testing
#'
#' A dataset containing sample security input data for 492 securities and 65
#' weekdays, from 2020-06-01 to 2020-08-31. Data items include average trading
#' dollar volume, market cap, and normalized size and value factors. The pricing
#' data used to construct the dataset was downloaded using the
#' \href{https://api.tiingo.com/}{Tiingo Stock API} and is used with permission.
#' Fundamental data items were downloaded from EDGAR.
#'
#' Data for most members of the S&P 500 are present. Some securities have been
#' omitted due to data processing complexities. For example, securities for
#' companies with multiple share classes have been omitted in the current
#' version.
#'
#' Values for shares outstanding and stockholders' equity downloaded from EDGAR may be
#' inaccurate due to XBRL parsing issues.
#'
#' Full code for reconstructing the dataset can be found in the
#' \href{https://github.com/strand-tech/pystrand}{pystrand} repository.
#'
#' @format A data frame with 31980 rows and 7 variables: 
#' \describe{
#'   \item{date}{Input date. It is assumed that the input data for day X is
#'   known at the beginning of day X (e.g., the data is as-of the previous day's
#'   close).}
#'   \item{id}{Security identifier.}
#'   \item{rc_vol}{Average dollar
#'   trading volume for the security over the past 20 trading days.}
#'   \item{market_cap}{Market capitalization, in dollars. The shares outstanding
#'   value used to calculate market cap is the latest value available at the
#'   beginning of the month.}
#'   \item{book_to_price}{Ratio of total equity to
#'   market cap. The stockholders' equity value used to calculate book to price
#'   is the latest value available at the beginning of the month.}
#'   \item{size}{Market cap factor normalized to be N(0,1) for each day.}
#'   \item{value}{Book to price factor normalized to be N(0,1) for each day.} }
#' @usage data(sample_inputs)
"sample_inputs"

#' Sample security pricing data for examples and testing
#'
#' A dataset containing sample security pricing data for 492 securities and 65
#' weekdays, from 2020-06-01 to 2020-08-31. This data was downloaded using the
#' \href{https://api.tiingo.com/}{Tiingo Stock API} and is redistributed with
#' permission.
#'
#' Full code for reconstructing the dataset can be found in the
#' \href{pystrand}{https://github.com/strand-tech/pystrand} repository.
#'
#' @format A data frame with 31980 rows and 8 variables:
#' \describe{
#'   \item{date}{Pricing date.}
#'   \item{id}{Security identifier.}
#'   \item{price_unadj}{The unadjusted price of the security.}
#'   \item{prior_close_unadj}{The unadjusted prior closing price of the security.}
#'   \item{dividend_unadj}{The dividend for the security on an unadjusted basis, if any.}
#'   \item{distribution_unadj}{The distribution (e.g., spin-off) for the
#'   security on an unadjusted basis (note that there is no spin-off information
#'   in this dataset, so all values are zero).}
#'   \item{volume}{Trading volume for the security, in shares.}
#'   \item{adjustment_ratio}{The adjustment ratio for the security. For example,
#'   AAPL has an adjustment ratio of 0.25 to account for its 4:1 split on
#'   2020-08-31.}
#' }
#' @usage data(sample_pricing)
"sample_pricing"

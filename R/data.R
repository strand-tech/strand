#' Sample security reference data for examples and testing
#'
#' A dataset containing sample (fake) security reference data for 500 securities.
#'
#' @format A data frame with 500 rows and 4 variables:
#' \describe{
#'   \item{id}{security identifier}
#'   \item{symbol}{human-readable trading symbol}
#'   \item{category_1}{categorical variable with values A-F}
#'   \item{category_2}{categorical variable with values A-L}
#' }
"sample_secref"

#' Sample security inputs for examples and testing
#'
#' A dataset containing sample (fake) security input data for 500 securities and
#' 63 weekdays, from 2019-01-02 to 2019-03-29.
#'
#' @format A data frame with 31500 rows and 9 variables:
#' \describe{
#'   \item{id}{security identifier}
#'   \item{date}{input date}
#'   \item{average_volume}{measurement of average security trading volume, in shares}
#'   \item{alpha_1}{sample numeric alpha input}
#'   \item{alpha_2}{sample numeric alpha input}
#'   \item{factor_1}{sample numeric factor input}
#'   \item{factor_2}{sample numeric factor input}
#'   \item{factor_3}{sample numeric factor input}
#'   \item{factor_4}{sample numeric factor input}
#' }
"sample_inputs"

#' Sample pricing data for examples and testing
#'
#' A dataset containing sample (fake) pricing data for 500 securities and
#' 63 weekdays, from 2019-01-02 to 2019-03-29.
#'
#' @format A data frame with 31500 rows and 8 variables:
#' \describe{
#'   \item{id}{security identifier}
#'   \item{date}{pricing date}
#'   \item{price_unadj}{the unadjusted price of the security}
#'   \item{prior_close_unadj}{the unadjusted prior closing price of the security}
#'   \item{dividend_unadj}{the dividend for the security on an unadjusted basis}
#'   \item{distribution_unadj}{the distribution (e.g., spin-off) for the security on an unadjusted basis}
#'   \item{volume}{trading volume for the security, in shares}
#'   \item{adjustment_ratio}{the adjustment ratio for the security}
#' }
"sample_pricing"

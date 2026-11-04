#' A real estate example dataframe
#'
#' A subset of data from a fictitious real estate data frame containing transaction prices and some
#' categorical and numerical characteristics of each dwelling.
#'
#' @format A data frame with 7,800 rows and 6 columns:
#' \describe{
#'   \item{period}{A (string) vector indicating a time period}
#'   \item{price}{A (string) vector indicating the transaction price of the dwelling}
#'   \item{floor_area}{A real-valued vector of (the logarithm of) the floor area of the dwelling}
#'   \item{dist_trainstation}{A real-valued vector of (the logarithm of) the distance of the dwelling to the nearest train station}
#'   \item{neighbourhood_code}{A categorical code/string referring to the neighbourhood the dwelling belongs to}
#'   \item{dummy_large_city}{A vector indicating whether the dwelling belongs to a large city or not}
#' }
#' @source A fictitious dataset for illustration purposes
#'
#' @examples
#' data(hedonic_data)
#' head(hedonic_data)
"hedonic_data"

#' A sales price appraisal ratio example data frame
#'
#' A compact SPAR development fixture with two appraisal vintages, five property
#' types, twelve monthly periods per vintage, and three transactions per
#' period/property-type combination. The established name `aritmethic_data` is
#' retained for compatibility with SPAR development examples.
#'
#' @format A data frame with 360 rows and 5 columns:
#' \describe{
#'   \item{Period}{An integer monthly period in YYYYMM format.}
#'   \item{Appraisal Year}{An integer appraisal vintage.}
#'   \item{Price}{A numeric transaction price.}
#'   \item{Property Type}{A character property-type code.}
#'   \item{Appraisal Value}{A numeric appraisal value.}
#' }
#' @source SPAR example data supplied for REPS development, retained from the
#'   reference branch at commit 6675d8a.
#' @examples
#' data("aritmethic_data")
#' result <- calculate_spar(
#'   aritmethic_data, "arithmetic", period_variable = "Period",
#'   dependent_variable = "Price", appraisal_variable = "Appraisal Value",
#'   grouping_variables = c("Appraisal Year", "Property Type")
#' )
#' head(result)
"aritmethic_data"


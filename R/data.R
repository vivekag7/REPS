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

#' A compact sales price appraisal ratio example dataset
#'
#' A deterministic, representative subset of the original SPAR development data
#' spanning January 2013 through December 2018, with six appraisal vintages and
#' four property types. Each period/vintage/type stratum retains up to three
#' observations at approximately quartile-spaced price ranks, with appraisal
#' value breaking ties. All periods and vintage transitions are retained.
#' Intended for examples, tests, and demonstrating chaining over vintages.
#' The selection is reproducible with `data-raw/arithmetic_data.R` and the
#' external source CSV; the full source is not distributed with the package.
#'
#' @format A data frame with 857 rows and 5 columns:
#' \describe{
#'   \item{period}{An integer monthly period in YYYYMM format.}
#'   \item{appraisal_year}{The integer WOZ/appraisal-vintage year on which the appraisal value is based.}
#'   \item{price}{A numeric transaction price.}
#'   \item{property_type}{A character housing/property category.}
#'   \item{appraisal_value}{A numeric appraisal/WOZ value.}
#' }
#' @source A compact subset of the original SPAR development/test file `test2.csv`.
#' @examples
#' data("arithmetic_data")
#' result <- calculate_spar(
#'   arithmetic_data, "arithmetic", period_variable = "period",
#'   dependent_variable = "price", appraisal_variable = "appraisal_value",
#'   grouping_variables = c("appraisal_year", "property_type"),
#'   index_type_variable = "property_type", chained = TRUE
#' )
#' head(result)
"arithmetic_data"


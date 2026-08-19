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
#' A deterministic subset of the SPAR development data. It contains two
#' appraisal years, five property types, twelve monthly periods per appraisal
#' year, and three observations per period/property-type combination. The name
#' `aritmethic_data` follows the spelling requested for this package dataset.
#'
#' @format A data frame with 360 rows and 5 columns:
#' \describe{
#'   \item{Period}{An integer identifying the monthly period in `YYYYMM` format.}
#'   \item{Appraisal Year}{An integer identifying the appraisal reference year.}
#'   \item{Price}{A numeric transaction price.}
#'   \item{Property Type}{A character property-type code.}
#'   \item{Appraisal Value}{A numeric appraisal value.}
#' }
#' @source A compact SPAR example dataset supplied for package development.
#'
#' @examples
#' data(aritmethic_data)
#'
#' calculate_spar(
#'   dataset = aritmethic_data,
#'   method = "arithmetic",
#'   period_variable = "Period",
#'   dependent_variable = "Price",
#'   appraisal_variable = "Appraisal Value",
#'   grouping_variables = c("Appraisal Year", "Property Type")
#' )
"aritmethic_data"


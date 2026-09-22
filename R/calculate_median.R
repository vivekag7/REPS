#' Calculate Median Price Index
#'
#' Internal unadjusted comparison method used by `calculate_hedonic_index()`.
#'
#' @return A standardized index data frame.
#' @importFrom stats median
#' @keywords internal
#' @noRd
calculate_median <- function(dataset,
                             period_variable,
                             dependent_variable,
                             numerical_variables = NULL,
                             categorical_variables = NULL,
                             reference_period = NULL,
                             number_of_observations = TRUE) {
  periods <- sort(unique(as.character(dataset[[period_variable]])))

  median_prices <- vapply(periods, function(period) {
    values <- dataset[
      as.character(dataset[[period_variable]]) == period,
      dependent_variable
    ]
    stats::median(values, na.rm = TRUE)
  }, numeric(1))

  # Transform the series of median prices into an index.
  index <- calculate_index(
    periods = periods,
    values = median_prices,
    reference_period = reference_period
  )

  observation_counts <- NULL
  if (isTRUE(number_of_observations)) {
    observation_counts <- vapply(periods, function(period) {
      values <- dataset[
        as.character(dataset[[period_variable]]) == period,
        dependent_variable
      ]
      sum(!is.na(values))
    }, integer(1))
  }

  format_index_output(
    periods = periods,
    index_values = index,
    observation_counts = observation_counts
  )
}

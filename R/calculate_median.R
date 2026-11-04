#' Calculate an unadjusted median-price baseline
#'
#' Uses the untransformed dependent variable and complete price-period pairs.
#' Explanatory variables are ignored, including their missing values.
#'
#' @inheritParams calculate_hedonic_index
#' @return A data frame with period, optional observation counts, and Index.
#' @keywords internal
#' @noRd
calculate_median <- function(dataset,
                             period_variable,
                             dependent_variable,
                             numerical_variables = NULL,
                             categorical_variables = NULL,
                             reference_period = NULL,
                             number_of_observations = FALSE) {
  clean_data <- prepare_hedonic_data(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    numerical_variables = NULL,
    categorical_variables = NULL
  )

  if (nrow(clean_data) == 0L) {
    stop("Median index calculation requires at least one complete price-period observation.")
  }

  prices_by_period <- split(
    clean_data[[dependent_variable]],
    clean_data[[period_variable]]
  )
  median_prices <- vapply(prices_by_period, stats::median, numeric(1))
  periods <- names(median_prices)
  index_values <- calculate_index(periods, median_prices, reference_period)
  observation_counts <- if (number_of_observations) lengths(prices_by_period) else NULL

  format_index_output(
    periods = periods,
    index_values = index_values,
    observation_counts = observation_counts
  )
}

#' Calculate Rolling Time Dummy Index
#'
#' Estimates a price index using rolling windows of time dummy regressions.
#'
#' @author Vivek Gajadhar
#' @param dataset data frame with input data
#' @param period_variable name of the time variable (string)
#' @param dependent_variable name of the dependent variable (usually price, assumed unlogged)
#' @param numerical_variables vector of numeric quality-determining variables
#' @param categorical_variables vector of categorical variables
#' @param reference_period period to be normalized to index = 100 (e.g., "2015")
#' @param window_length length of each rolling window (integer)
#' @param number_of_observations logical, whether to return number of observations per period (default = FALSE)
#' @param parallel Logical; whether independent rolling-window models are parallelized.
#' @return data frame with period, Index, and optionally number_of_observations
#' @importFrom stats setNames
#' @importFrom utils tail
#' @keywords internal
#' @noRd

calculate_rolling_timedummy <- function(dataset,
                                        period_variable,
                                        dependent_variable,
                                        numerical_variables,
                                        categorical_variables,
                                        reference_period,
                                        window_length,
                                        number_of_observations = FALSE,
                                        parallel = FALSE) {
  # Get all periods sorted chronologically
  period_values <- as.character(dataset[[period_variable]])
  periods_all <- sort(unique(period_values))
  
  growth_rates <- numeric(length(periods_all))
  names(growth_rates) <- periods_all
  last_window_start <- length(periods_all) - window_length + 1

  calculate_window_index <- function(start) {
    window_periods <- periods_all[start:(start + window_length - 1)]
    window_data <- dataset[period_values %in% window_periods, , drop = FALSE]

    calculate_time_dummy(
        dataset = window_data,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        numerical_variables = numerical_variables,
        categorical_variables = categorical_variables
    )
  }

  window_starts <- seq_len(last_window_start)
  window_results <- run_parallel_tasks(
    tasks = window_starts,
    task_function = calculate_window_index,
    parallel = parallel,
    fallback_message = "Parallel rolling time dummy calculation failed; falling back to sequential calculation."
  )

  for (i in seq_along(window_results)) {
    window_index <- window_results[[i]]
    window_growth_rates <- calculate_growth_rate(stats::setNames(window_index$Index, window_index$period))

    if (i == 1) {
      growth_rates[window_index$period] <- window_growth_rates
    } else {
      last_period <- utils::tail(window_index$period, 1)
      growth_rates[last_period] <- utils::tail(window_growth_rates, 1)
    }
  }
  
  # Build final index series based on chained growth rates
  index_values <- calculate_index(periods_all, cumprod(growth_rates) * 100, reference_period)
  
  # Optionally add number of observations
  obs_counts <- NULL
  if (number_of_observations) {
    counts <- table(period_values)
    obs_counts <- as.integer(counts[periods_all])
  }
  
  format_index_output(
    periods = periods_all,
    index_values = index_values,
    reference_period = NULL,
    observation_counts = obs_counts
  )
}

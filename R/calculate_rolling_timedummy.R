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
#' @return data frame with period, Index, and optionally number_of_observations
#' @importFrom stats setNames
#' @importFrom utils tail
#' @keywords internal

calculate_rolling_timedummy <- function(dataset,
                                              period_variable,
                                              dependent_variable,
                                              numerical_variables,
                                              categorical_variables,
                                              reference_period,
                                              window_length,
                                              number_of_observations = FALSE) {
  # Get all periods sorted chronologically
  periods_all <- sort(unique(as.character(dataset[[period_variable]])))
  
  # First rolling window
  initial_window_periods <- periods_all[1:window_length]
  window_data <- dataset[dataset[[period_variable]] %in% initial_window_periods, ]
  
  # Run time dummy index for initial window
  initial_index <- calculate_time_dummy(
    dataset = window_data,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    numerical_variables = numerical_variables,
    categorical_variables = categorical_variables
  )
  
  # Convert index to growth rates
  growth_rates <- calculate_growth_rate(setNames(initial_index$Index / 100, initial_index$period))
  
  # Loop through remaining rolling windows
  window_starts <- 2:(length(periods_all) - window_length + 1)
  for (start in window_starts) {
    window_periods <- periods_all[start:(start + window_length - 1)]
    window_data <- dataset[dataset[[period_variable]] %in% window_periods, ]
    
    # Calculate index for new window
    new_index <- calculate_time_dummy(
      dataset = window_data,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      numerical_variables = numerical_variables,
      categorical_variables = categorical_variables
    )
    
    # Append last growth rate from new window
    last_growth <- tail(calculate_growth_rate(setNames(new_index$Index / 100, new_index$period)), 1)
    growth_rates <- c(growth_rates, setNames(last_growth, tail(new_index$period, 1)))
  }
  
  # Build final index series based on chained growth rates
  df_result <- data.frame(period = periods_all)
  df_result$Index <- calculate_index(df_result$period, growth_rates, reference_period)
  
  # Optionally add number of observations
  if (number_of_observations) {
    counts <- table(dataset[dataset[[period_variable]] %in% periods_all, period_variable])
    obs_df <- data.frame(period = names(counts), number_of_observations = as.integer(counts))
    df_result <- merge(df_result, obs_df, by = "period", all.x = TRUE)
    
    # Reorder columns
    df_result <- df_result[, c("period", "number_of_observations", "Index")]
    
    }
  
    
  return(df_result)
}

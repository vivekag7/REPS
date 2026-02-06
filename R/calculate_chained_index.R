#' Calculate a Chained Index (Annual Overlap Method)
#'
#' Calculates a price index by chaining short-term indices. The method splits the data into years.
#' For the first year, a standard index is calculated. For subsequent years, an index is calculated
#' including the last period of the previous year (the overlap period). These short-term indices
#' are then linked (chained) together to form a continuous series.
#'
#' This approach is often used to update index weights annually or to reduce the impact of
#' structural changes in the market over long periods.
#'
#' @author Vivek Gajadhar
#' @param dataset Data frame with input data
#' @param method The index method to use for the short-term segments (e.g., "fisher", "timedummy", "hmts")
#' @param period_variable Name of the column containing time periods (e.g., "2020Q1", "2020M01")
#' @param dependent_variable Name of the dependent variable (usually price)
#' @param numerical_variables Vector of numeric quality-determining variables (default = NULL)
#' @param categorical_variables Vector of categorical variables (default = NULL)
#' @param reference_period Period to normalize the final index to (e.g., "2015")
#' @param ... Additional arguments passed to the underlying calculate_price_index method (e.g. window_length)
#' @return A data.frame with the chained index series
#' @export
#' @importFrom dplyr filter bind_rows mutate select arrange
#' @importFrom stats na.omit
#' @importFrom utils tail

calculate_chained_index <- function(dataset,
                                    method,
                                    period_variable,
                                    dependent_variable,
                                    numerical_variables = NULL,
                                    categorical_variables = NULL,
                                    reference_period = NULL,
                                    ...) {
  
  # Validate inputs using the existing validation function
  validate_input(dataset, period_variable, dependent_variable, numerical_variables, categorical_variables)
  
  # Extract periods and determine structure
  # We need to parse the period string to identify Years and sequences
  periods_raw <- as.character(dataset[[period_variable]])
  unique_periods <- sort(unique(periods_raw))
  
  # Helper to parse year from string (expects YYYY at start)
  get_year <- function(p) {
    as.integer(substr(p, 1, 4))
  }
  
  years <- unique(get_year(unique_periods))
  
  # Storage for the short-term indices
  short_term_results <- list()
  
  # --- Step 1: Calculate Index for the First Year ---
  first_year <- min(years)
  first_year_periods <- unique_periods[get_year(unique_periods) == first_year]
  
  data_subset_first <- dataset[dataset[[period_variable]] %in% first_year_periods, ]
  
  # For the first year, we calculate the index normally
  index_first <- calculate_price_index(
    dataset = data_subset_first,
    method = method,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    numerical_variables = numerical_variables,
    categorical_variables = categorical_variables,
    reference_period = NULL, # Internal base, will be chained later
    number_of_observations = FALSE,
    ...
  )
  
  # Store first year result
  short_term_results[[as.character(first_year)]] <- index_first
  
  # --- Step 2: Loop through subsequent years ---
  if (length(years) > 1) {
    for (i in 2:length(years)) {
      current_year <- years[i]
      prev_year <- years[i-1]
      
      # Identify periods for current year
      current_periods <- unique_periods[get_year(unique_periods) == current_year]
      
      # Identify overlap period: The last period of the previous year
      prev_periods <- unique_periods[get_year(unique_periods) == prev_year]
      overlap_period <- tail(sort(prev_periods), 1)
      
      # Define the calculation window: Overlap period + Current year
      calculation_periods <- c(overlap_period, current_periods)
      data_subset <- dataset[dataset[[period_variable]] %in% calculation_periods, ]
      
      # Calculate index for this window
      # We set the reference_period to the overlap_period to make linking easier (Overlap = 100)
      index_current <- calculate_price_index(
        dataset = data_subset,
        method = method,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        numerical_variables = numerical_variables,
        categorical_variables = categorical_variables,
        reference_period = overlap_period, 
        number_of_observations = FALSE,
        ...
      )
      
      # Remove the overlap period from the result to avoid duplicates when binding
      # (We only keep the new periods of the current year)
      index_current_clean <- index_current[index_current$period != overlap_period, ]
      
      short_term_results[[as.character(current_year)]] <- index_current_clean
    }
  }
  
  # --- Step 3: Chain the results ---
  
  # Combine all short term parts into one dataframe
  full_series <- dplyr::bind_rows(short_term_results)
  
  # Ensure chronological order
  full_series <- full_series[order(full_series$period), ]
  
  # Reconstruct the chain
  final_index <- numeric(nrow(full_series))
  periods_vec <- full_series$period
  
  # Handle Year 1
  n_y1 <- length(first_year_periods)
  # Normalize Year 1 to start at 100 (arbitrary start, will be rescaled later)
  final_index[1:n_y1] <- short_term_results[[1]]$Index / short_term_results[[1]]$Index[1] * 100
  
  current_idx <- n_y1 + 1
  
  if (length(short_term_results) > 1) {
    for (i in 2:length(short_term_results)) {
      # Get the index values for this year (which are based on prev_year_end = 100)
      factors <- short_term_results[[i]]$Index / 100
      
      # The level to multiply by is the FINAL index value of the previous period
      previous_level <- final_index[current_idx - 1]
      
      # Calculate new levels
      n_obs <- length(factors)
      final_index[current_idx:(current_idx + n_obs - 1)] <- factors * previous_level
      
      current_idx <- current_idx + n_obs
    }
  }
  
  # Create result dataframe
  result_table <- data.frame(
    period = periods_vec,
    Index = final_index
  )
  
  # --- Step 4: Re-reference ---
  # Normalize to the user-requested reference period
  if (!is.null(reference_period)) {
    result_table$Index <- calculate_index(result_table$period, result_table$Index, reference_period)
  }
  
  return(result_table)
}
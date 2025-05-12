#' Calculate repricing index based on hedonic model (geometric adjustment)
#'
#' For each pair of subsequent periods, this method compares the observed geometric mean price
#' with the predicted mean price from a hedonic regression model. The ratio of these two values
#' forms the basis of the repricing growth rate, which is then accumulated into an index.
#'
#' @author Vivek Gajadhar
#' @param dataset a data frame containing the data
#' @param period_variable character name of the time period variable
#' @param dependent_variable character name of the dependent variable (e.g., sale price)
#' @param continuous_variables character vector of numeric quality-determining variables
#' @param categorical_variables character vector of categorical variables (including dummies)
#' @param reference_period reference period (numeric or string) to normalize index to 100
#' @param number_of_observations logical, if TRUE, adds number of observations column
#' @return a data.frame with columns: period, Index, (optionally number_of_observations)
#' @keywords internal
#' @importFrom dplyr %>% rename mutate filter group_by summarise all_of across
#' @importFrom stats lm predict as.formula

calculate_repricing <- function(dataset,
                                period_variable,
                                dependent_variable,
                                continuous_variables,
                                categorical_variables,
                                reference_period = NULL,
                                number_of_observations = FALSE) {
  
  # Combine independent variables
  independent_variables <- c(continuous_variables, categorical_variables)
  
  # Check if required variables exist
  required_vars <- c(period_variable, dependent_variable, independent_variables)
  stopifnot(all(required_vars %in% names(dataset)))
  
  # Rename period variable and apply log to dependent
  dataset <- dataset %>%
    dplyr::rename(period = all_of(period_variable)) %>%
    dplyr::mutate(
      period = as.character(period),
      log_depvar = log(.data[[dependent_variable]]),
      dplyr::across(dplyr::all_of(categorical_variables), as.factor)
    )
  
  # Sorted unique periods
  period_list <- sort(unique(dataset$period), decreasing = FALSE)
  
  results <- data.frame(period = period_list)
  growth_rate <- numeric(length(period_list))
  growth_rate[1] <- 1  # set base period growth rate
  
  if (number_of_observations) {
    num_obs <- numeric(length(period_list))
    num_obs[1] <- nrow(dataset %>% filter(period == period_list[1]))
  }
  
  for (t in 2:length(period_list)) {
    
    # Subset 2-period window
    relevant_periods <- period_list[(t - 1):t]
    subset_data <- dataset %>% filter(period %in% relevant_periods)
    
    # Build formula and fit model
    formula_str <- paste0("log_depvar ~ ", paste(independent_variables, collapse = " + "))
    model <- lm(as.formula(formula_str), data = subset_data)
    
    # Get mean characteristics per period
    average_data <- subset_data %>%
      group_by(period) %>%
      summarise(
        observed_gmean = exp(mean(log(.data[[dependent_variable]]), na.rm = TRUE)),
        across(all_of(continuous_variables), \(x) mean(x, na.rm = TRUE)),
        across(all_of(categorical_variables), ~ names(sort(table(.), decreasing = TRUE))[1]),
        .groups = "drop"
      )
    
    # Predict mean price using model
    average_data$predicted_price <- exp(predict(model, newdata = average_data))
    
    # Compute growth ratio
    observed_ratio <- average_data$observed_gmean[2] / average_data$observed_gmean[1]
    predicted_ratio <- average_data$predicted_price[2] / average_data$predicted_price[1]
    
    growth_rate[t] <- observed_ratio / predicted_ratio
    
    if (number_of_observations) {
      num_obs[t] <- nrow(subset_data %>% filter(period == period_list[t]))
    }
  }
  
  # Finalize index
  Index <- cumprod(growth_rate) * 100
  
  # Construct final result table with correct column order
  if (number_of_observations) {
    results <- data.frame(
      period = period_list,
      number_of_observations = num_obs,
      Index = Index
    )
  } else {
    results <- data.frame(
      period = period_list,
      Index = Index
    )
  }
  
  # Normalize index to reference period 
  results$Index <- calculate_index(results$period, results$Index, reference_period)
  
  return(results)
}

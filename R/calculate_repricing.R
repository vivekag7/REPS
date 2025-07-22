#' Calculate repricing index based on hedonic model (geometric adjustment)
#'
#' For each pair of subsequent periods, this method compares the observed geometric mean price
#' with the predicted mean price from a hedonic regression model. The ratio of these two values
#' forms the basis of the repricing growth rate, which is then accumulated into an index.
#'
#' @author Vivek Gajadhar, Farley Ishaak
#' @param dataset a data frame containing the data
#' @param period_variable character name of the time period variable
#' @param dependent_variable character name of the dependent variable (e.g., sale price)
#' @param numerical_variables character vector of numeric quality-determining variables
#' @param categorical_variables character vector of categorical variables (including dummies)
#' @param periods_in_year if month, then 12. If quarter, then 4, etc. (default = 4)
#' @param reference_period reference period (numeric or string) to normalize index to 100
#' @param number_of_observations logical, if TRUE, adds number of observations column
#' @return a data.frame with columns: period, Index, (optionally number_of_observations)
#' @keywords internal
#' @importFrom stats lm predict as.formula aggregate

calculate_repricing <- function(dataset,
                                period_variable,
                                dependent_variable,
                                numerical_variables,
                                categorical_variables,
                                reference_period = NULL,
                                number_of_observations = FALSE,
                                periods_in_year = 4) {
  
  internal_period <- ".index_period_internal"
  independent_variables <- c(numerical_variables, categorical_variables)
  
  # Check if required variables exist
  required_vars <- c(period_variable, dependent_variable, independent_variables)
  stopifnot(all(required_vars %in% names(dataset)))
  
  # Assign internal period column and prepare data
  dataset[[internal_period]] <- as.character(dataset[[period_variable]])
  
  dataset[["log_depvar"]] <- log(dataset[[dependent_variable]])
  for (var in categorical_variables) dataset[[var]] <- as.factor(dataset[[var]])
  
  # Sort unique periods
  period_list <- sort(unique(dataset[[internal_period]]), decreasing = FALSE)
  
  # Prepare table for results
  results <- data.frame(period = period_list)

  # Subset base year
  base_year <- period_list[c(1:periods_in_year)]
  subset_data_base <- dataset[dataset[[internal_period]] %in% base_year, , drop = FALSE]
  
  # Build formula
  formula_str <- paste0("log_depvar ~ ", paste(independent_variables, collapse = " + "))
  
  # Fit model period base year
  model_base <- lm(as.formula(formula_str), data = subset_data_base)
 
  # Predict mean price for observations in all periods using model for base year
  dataset$predicted_price <- exp(predict(model_base, newdata = dataset))
  
  # Calculate mean, sum and numbers per period
  average_data <- aggregate(dataset[[dependent_variable]], 
                            by = list(dataset[[internal_period]]), 
                            FUN = function(x) exp(mean(log(x), na.rm = TRUE)))
  names(average_data) <- c(internal_period, "observed_gmean")
  
  predicted_means <- tapply(log(dataset[["predicted_price"]]), 
                            dataset[[internal_period]], 
                            mean, na.rm = TRUE)
  average_data$predicted_price <- exp(predicted_means[average_data[[internal_period]]])
  
  counts <- table(dataset[[internal_period]])
  average_data$num_obs <- as.integer(counts[average_data[[internal_period]]])
  
  
  # Calculate index
  average_data$index <- (average_data$observed_gmean / average_data$observed_gmean[1]) /
                        (average_data$predicted_price / average_data$predicted_price[1]) * 100
  
  # Construct final result table with correct column order
  if (number_of_observations) {
    results <- data.frame(
      period = period_list,
      number_of_observations = average_data$num_obs,
      Index = average_data$index
    )
  } else {
    results <- data.frame(
      period = period_list,
      Index = average_data$index
    )
  }
  
  # Normalize index to reference period 
  results$Index <- calculate_index(results$period, results$Index, reference_period)
  
  return(results)
}

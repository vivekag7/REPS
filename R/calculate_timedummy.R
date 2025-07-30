#' Calculate Time Dummy Index
#'
#' Estimates a price index using a single regression with time dummy variables.
#'
#' @author Vivek Gajadhar
#' @param dataset data frame with input data
#' @param period_variable name of the time variable (string)
#' @param dependent_variable name of the dependent variable (usually price, assumed unlogged)
#' @param numerical_variables vector of numeric quality-determining variables
#' @param categorical_variables vector of categorical variables
#' @param reference_period period to be normalized to index = 100 (e.g., "2015")
#' @param number_of_observations logical, whether to return number of observations per period (default = FALSE)
#' @return data frame with period, Index, and optionally number_of_observations
#' @importFrom stats lm coefficients as.formula na.omit
#' @importFrom utils tail
#' @keywords internal

calculate_time_dummy <- function(dataset,
                                       period_variable,
                                       dependent_variable,
                                       numerical_variables,
                                       categorical_variables,
                                       reference_period = NULL,
                                       number_of_observations = FALSE) {
  
  # Convert categorical vars and period to factors, and log-transform dependent and numerical vars
  for (var in c(categorical_variables, period_variable)) dataset[[var]] <- as.factor(dataset[[var]])
  dataset[[dependent_variable]] <- log(dataset[[dependent_variable]])
  for (var in numerical_variables) dataset[[var]] <- log(dataset[[var]])
  
  
  # Keep only relevant variables and drop rows with NA
  variables_to_use <- c(dependent_variable, numerical_variables, categorical_variables, period_variable)
  calculation_data <- dataset[, variables_to_use, drop = FALSE]
  calculation_data <- na.omit(calculation_data)
  calculation_data[] <- lapply(calculation_data, function(x) if (is.factor(x)) droplevels(x) else x)
  
  
  # Build regression formula and fit model
  formula <- stats::as.formula(paste(dependent_variable, "~", paste(c(numerical_variables, categorical_variables, period_variable), collapse = " + ")))
  model <- stats::lm(formula, data = calculation_data)
  
  # Extract time dummy coefficients
  coefs <- stats::coefficients(model)
  period_levels <- levels(dataset[[period_variable]])
  log_time_dummies <- setNames(rep(0, length(period_levels)), period_levels)
  time_dummy_names <- grep(paste0("^", period_variable), names(coefs), value = TRUE)
  
  for (name in time_dummy_names) {
    level <- sub(paste0(period_variable), "", name)
    log_time_dummies[level] <- coefs[name]
  }
  
  # Convert log-index to standard index (base = 100)
  index <- exp(log_time_dummies) * 100
  
  # Create index dataframe
  df_index <- data.frame(period = names(index), Index = as.numeric(index))
  
  # Add number of observations if requested
  if (number_of_observations) {
    counts <- table(calculation_data[[period_variable]])
    obs_df <- data.frame(period = names(counts), number_of_observations = as.integer(counts))
    df_index <- merge(df_index, obs_df, by.x = "period", by.y = "period", all.x = TRUE)
  }
  
  
  # Normalize index to reference period if provided
  if (!is.null(reference_period)) {
    df_index$Index <- calculate_index(df_index$period, df_index$Index, reference_period)
  }
  
  # Reorder columns if observations included
  if (number_of_observations) {
    df_index <- df_index[, c("period", "number_of_observations", "Index")]
  }
  
  return(df_index)
}

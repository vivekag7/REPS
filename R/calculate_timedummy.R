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
#' @importFrom dplyr mutate across all_of select group_by summarise left_join n
#' @keywords internal

calculate_time_dummy <- function(dataset,
                                       period_variable,
                                       dependent_variable,
                                       numerical_variables,
                                       categorical_variables,
                                       reference_period = NULL,
                                       number_of_observations = FALSE) {
  # Convert categorical vars and period to factors, and log-transform dependent and numerical vars
  dataset <- dataset |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(categorical_variables, period_variable)), as.factor)) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(dependent_variable), log),
      dplyr::across(dplyr::all_of(numerical_variables), log)
    )
  
  # Keep only relevant variables and drop rows with NA
  variables_to_use <- c(dependent_variable, numerical_variables, categorical_variables, period_variable)
  calculation_data <- dataset |>
    dplyr::select(dplyr::all_of(variables_to_use)) |>
    stats::na.omit() |>
    droplevels()
  
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
    df_index <- df_index |>
      dplyr::left_join(
        calculation_data |>
          dplyr::group_by(.data[[period_variable]]) |>
          dplyr::summarise(number_of_observations = dplyr::n(), .groups = "drop"),
        by = c("period" = period_variable)
      )
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

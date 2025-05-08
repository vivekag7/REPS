#' Calculate Time Dummy Index
#'
#' Estimates a price index using a single regression with time dummy variables.
#'
#' @author Vivek Gajadhar
#' @param dataset data frame with input data
#' @param period_variable name of the time variable (string)
#' @param dependent_variable name of the dependent variable (usually price, assumed unlogged)
#' @param continuous_variables vector of numeric quality-determining variables
#' @param categorical_variables vector of categorical variables
#' @param reference_period period to be normalized to index = 100 (e.g., "2015")
#' @param number_of_observations logical, whether to return number of observations per period (default = FALSE)
#' @return data frame with period, Index, and optionally number_of_observations
#' @importFrom stats lm coefficients as.formula na.omit
#' @importFrom utils tail
#' @importFrom dplyr mutate across all_of select group_by summarise left_join n
#' @keywords internal

calculate_time_dummy_index <- function(dataset,
                                       period_variable,
                                       dependent_variable,
                                       continuous_variables,
                                       categorical_variables,
                                       reference_period = NULL,
                                       number_of_observations = FALSE) {
  dataset <- dataset |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(categorical_variables, period_variable)), as.factor)) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(dependent_variable), log),
      dplyr::across(dplyr::all_of(continuous_variables), log)
    )
  
  variables_to_use <- c(dependent_variable, continuous_variables, categorical_variables, period_variable)
  calculation_data <- dataset |>
    dplyr::select(dplyr::all_of(variables_to_use)) |>
    stats::na.omit() |>
    droplevels()
  
  formula <- stats::as.formula(paste(dependent_variable, "~", paste(c(continuous_variables, categorical_variables, period_variable), collapse = " + ")))
  model <- stats::lm(formula, data = calculation_data)
  
  num_periods <- length(unique(dataset[[period_variable]]))
  exp_coefs <- exp(stats::coefficients(model))
  exp_time_dummies <- tail(exp_coefs, num_periods - 1)
  index <- c(100, exp_time_dummies * 100)
  period_names <- levels(as.factor(dataset[[period_variable]]))
  names(index) <- period_names
  
  df_index <- data.frame(period = names(index), Index = as.numeric(index))
  
  if (number_of_observations) {
    df_index <- df_index |>
      dplyr::left_join(
        calculation_data |>
          dplyr::group_by(.data[[period_variable]]) |>
          dplyr::summarise(number_of_observations = dplyr::n(), .groups = "drop"),
        by = c("period" = period_variable)
      )
  }
  
  if (!is.null(reference_period)) {
    df_index$Index <- calculate_index(df_index$period, df_index$Index, reference_period)
  }
  
  # Reorder columns to match HMTS style
  if (number_of_observations) {
    df_index <- df_index[, c("period", "number_of_observations", "Index")]
  }
  
  return(df_index)
}
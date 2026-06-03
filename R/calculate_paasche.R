#' Calculate direct index according to the Paasche hedonic double imputation method
#'
#' By the parameters 'dependent_variable', 'continue_variable' and 'categorical_variables' as regression model is compiled.
#' With the model, a direct series of index figures is estimated by use of hedonic regression.
#'
#' N.B.: the independent variables must be entered transformed (and ready) in the parameters.
#' Hence, not: log(floor_area), but transform the variable in advance and then provide log_floor_area.
#' This does not count for the dependent variable. This should be entered untransformed
#'
#' Within the data, it is not necessary to filter the data on relevant variables or complete records.
#' This is taken care of in the function.
#'
#' @author Farley Ishaak, Vivek Gajadhar
#' @param dataset table with data (does not need to be a selection of relevant variables)
#' @param period_variable variable in the table with periods
#' @param dependent_variable usually the sale price
#' @param numerical_variables vector with quality determining numeric variables (no dummies)
#' @param categorical_variables vector with quality determining categorical variables (also dummies)
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param imputation display the underlying average imputation values? (default = FALSE)
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period
#' @keywords internal
#' @noRd
calculate_paasche <- function(dataset
                              , period_variable
                              , dependent_variable
                              , numerical_variables
                              , categorical_variables
                              , reference_period = NULL
                              , number_of_observations = FALSE
                              , imputation = FALSE) {

  # 1. PREPARE DATA
  clean_data <- prepare_hedonic_data(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    numerical_variables = numerical_variables,
    categorical_variables = categorical_variables,
    log_dependent = FALSE
  )
  independent_variables <- c(numerical_variables, categorical_variables)

  # Create list of periods
  period_list <- sort(unique(clean_data[[period_variable]]), decreasing = FALSE)
  number_of_periods <- number_of_periods_temp <- length(period_list)
  period_values <- clean_data[[period_variable]]

  # Prepare vector for index and numbers
  Index <- numeric(number_of_periods)
  number <- if (number_of_observations) integer(number_of_periods) else NULL
  imputation_matrix <- if (imputation) {
    matrix(
      NA_real_,
      nrow = number_of_periods,
      ncol = number_of_periods,
      dimnames = list(period_list, paste0("Base_", period_list))
    )
  } else {
    NULL
  }

  for (imputation_period in 1:number_of_periods) {

    # Select the last and first period
    period_list_paasche <- c(period_list[number_of_periods_temp], period_list[1])
    dataset_temp <- clean_data[period_values %in% period_list_paasche, , drop = FALSE]

    # Calculate Paasche imputations and numbers
    tbl_average_imputation <-
      calculate_hedonic_imputation(dataset_temp = dataset_temp
                                   , period_temp = period_variable
                                   , dependent_variable_temp = dependent_variable
                                   , independent_variables_temp = independent_variables
                                   , number_of_observations_temp = number_of_observations
                                   , period_list_temp = period_list_paasche)
    if (imputation == TRUE) {
      imputation_matrix[
        match(tbl_average_imputation$period, period_list),
        number_of_periods_temp
      ] <- tbl_average_imputation$average_imputation
    }
    

    if (number_of_observations == TRUE) {
      # Insert imputations into table
      number[imputation_period] <- tbl_average_imputation$number_of_observations[1]
    }

    # Insert last index figure into vector
    Index[imputation_period] <- tbl_average_imputation$average_imputation[1] / tbl_average_imputation$average_imputation[2] * 100

    # Stepwise delete last period
    number_of_periods_temp <- number_of_periods_temp - 1

  }

  # Reverse the index series (last period was calculated first)
  Index <- Index[imputation_period:1]
  if (number_of_observations == TRUE) {
    number <- number[imputation_period:1]
  }

  # 2. FORMAT OUTPUT
  obs_counts <- if (number_of_observations) number else NULL
  
  paasche <- format_index_output(
    periods = period_list,
    index_values = Index,
    reference_period = reference_period,
    observation_counts = obs_counts
  )

  # Safely handle the imputation matrix attachment
  if (imputation == TRUE) {
    imputation_source <- as.data.frame(imputation_matrix, check.names = FALSE)
    if (!number_of_observations) {
      imputation_source$period <- period_list
    }
    paasche$Imputation <- diag(as.matrix(imputation_source))
  }

  return(paasche)
}

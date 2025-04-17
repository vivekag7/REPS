
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
#' @author Farley Ishaak 
#' @param dataset table with data (does not need to be a selection of relevant variables)
#' @param period_variable variable in the table with periods
#' @param dependent_variable usually the sale price
#' @param continuous_variables vector with quality determining numeric variables (no dummies)
#' @param categorical_variables vector with quality determining categorical variables (also dummies)
#' @param index caprice index
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param imputation display the underlying average imputation values? (default = FALSE)
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period

calculate_paasche <- function(dataset
                              , period_variable
                              , dependent_variable
                              , continuous_variables
                              , categorical_variables
                              , reference_period = NULL
                              , index = TRUE
                              , number_of_observations = FALSE
                              , imputation = FALSE) {

  # Merge independent variables
  # independent_variables <- c(continuous_variables, categorical_variables)
  assertthat::assert_that(assertthat::has_name(dataset, c(period_variable, dependent_variable, continuous_variables, categorical_variables)))
  independent_variables <- c(continuous_variables, categorical_variables)

  # Rename period_variable and transform to character
  dataset <- dataset |>
    dplyr::rename(period_var_temp = all_of(period_variable)) |>
    dplyr::mutate(period_var_temp = as.character(period_var_temp),
                  dplyr::across(dplyr::all_of(categorical_variables),
                                as.factor))

  ## Calculate index

  # Create list of periods
  period_list <- sort(unique(dataset$period_var_temp), decreasing = FALSE)
  number_of_periods <- number_of_periods_temp <- length(period_list)

  # Prepare table for imputations
  tbl_imputations <- data.frame(period = period_list)

  # Prepare vector for index and numbers
  Index <- c(0)
  number <- c(0)

  for (imputation_period in 1:number_of_periods) {

    # Select the last and first period
    period_list_paasche <- c(period_list[number_of_periods_temp], period_list[1])
    dataset_temp <- dataset[which(dataset$period_var_temp %in% period_list_paasche), ]

    # Calculate Paasche imputations and numbers
    tbl_average_imputation <-
      calculate_hedonic_imputation(dataset_temp = dataset_temp
                                   , period_temp = "period_var_temp"
                                   , dependent_variable_temp = dependent_variable
                                   , independent_variables_temp = independent_variables
                                   , number_of_observations_temp = number_of_observations
                                   , period_list_temp = period_list_paasche)
    if (imputation == TRUE) {
      # Retain columns that are necessary for imputations
      tbl_merge <- tbl_average_imputation[, c("period", "average_imputation")]
      
      # Insert imputations into table
      tbl_imputations <- merge(tbl_imputations, tbl_merge, by = "period", all.x = TRUE)
      
      # Rename variable to base year
      names(tbl_imputations)[ncol(tbl_imputations)] <- paste0("Base_", period_list[number_of_periods_temp])
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
  number <- number[imputation_period:1]

  # Rescale the index
  if (!is.null(reference_period)) {
    Index <- calculate_index(period_list, Index, reference_period)
  }

  # Create table
  paasche <- data.frame(period = period_list)
  column_start <- 1

  if (number_of_observations == TRUE) {
    paasche$number_of_observations <- number
    column_start <- 2
  }
  if (index == TRUE) {
    paasche$Index <- Index
  }

  if (imputation == TRUE) {
    number_of_periods_plus_1 <- number_of_periods + 1
    tbl_imputations <- tbl_imputations[, c(1, number_of_periods_plus_1:column_start)] # Reverse the table (last period was calculated first)
    tbl_imputations <- unique.data.frame(tbl_imputations) # the base period was doubled
    paasche <- merge(paasche, tbl_imputations, by = "period")
  }

  return(paasche)
}

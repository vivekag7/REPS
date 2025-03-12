# This is the first main function (calculate laspeyres)

#' Calculate direct index according to the Laspeyres hedonic double imputation method
#'
#' By the parameters 'dependent_variable', 'continue_variable' and 'categorical_variables' as regression model is compiled.
#' With the model, a direct series of index figures is estimated by use of hedonic regression.
#'
#' N.B.: the independent variables must be entered transformed (and ready) in the parameters.
#' Hence, not: log(floor_area), but transform the variable in advance and then provide log_floor_area.
#' This does not count for the dependent variable. This should be entered untransformed/
#' The parameter log_dependent can be used to transform this variable.
#'
#' Within the data, it is not necessary to filter the data on relevant variables or complete records.
#' This is taken care of in the function.
#'
#' @author Farley Ishaak (FIHK)
#'
#' @param dataset table with data (does not need to be a selection of relevant variables)
#' @param period_variable variable in the table with periods
#' @param dependent_variable usually the sale price
#' @param continious_variables vector with quality determining numeric variables (no dummies)
#' @param categorical_variables vector with quality determining categorical variables (also dummies)
#' @param log_dependent should the dependent variable be transformed to its logarithm? default = TRUE
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param imputation display the inderlying average imputation values? (default = FALSE)
#' @param index caprice index
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr lag
#' @importFrom stats na.omit
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom stats runif
#' @importFrom assertthat assert_that
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period
#' @export
#' @examples
#'
#' Tbl_Laspeyres <- calculate_laspeyres(dataset = data_constraxion
#'                                 , period_variable = c("period")
#'                                 , dependent_variable = c('price')
#'                                 , continious_variables = c('floor_area')
#'                                 , categorical_variables = c('neighbourhood_code')
#'                                 , log_dependent = TRUE
#'                                 , reference_period = 2015
#'                                 , number_of_observations = TRUE
#'                                 , imputation = TRUE)
calculate_laspeyres <- function(dataset
                                , period_variable
                                , dependent_variable
                                , continious_variables
                                , categorical_variables
                                , log_dependent = FALSE
                                , reference_period = NULL
                                , index = TRUE
                                , number_of_observations = FALSE
                                , imputation = FALSE) {
  
  ### Controle toegevoegd om te kijken of aangewezen variabelen in de dataset
  ### voorkomen. Dit kan ook in een functie gedaan worden, aangezien het het ook voorkomt in paasche en fischer.
  # Merge independent variables
  # col_names <- names(dataset)
  # if (is.character(continious_variables) &&
  #     is.character(categorical_variables)) {
  #   independent_variables <- c(continious_variables, categorical_variables)
  #   if (!all(independent_variables %in% col_names) ||
  #       !(dependent_variable %in% col_names)) {
  #     stop("One or more of the variables is not present in the dataset.")
  #   }
  # } else {
  #   stop(paste("The input for the continious and categorical names",
  #               "must be the name of the columns in the dataframe."))
  # }
  
  ## assertthat voert alle hieboven beschreven checks uit.
  ## Check als dependent variable numeriek is? Ook voor cont vars?
  assertthat::assert_that(assertthat::has_name(dataset, c(period_variable, dependent_variable, continious_variables, categorical_variables)))
  independent_variables <- c(continious_variables, categorical_variables)
  
  
  # Rename period_variable and transform to character
  ### Omgezet naar tidyverse-style, een mix tussen base en tidyverse is inconsistent.
  dataset <- dataset |>
    dplyr::rename(period_var_temp = period_variable) |>
    dplyr::mutate(period_var_temp = as.character(period_var_temp),
                  dplyr::across(dplyr::all_of(categorical_variables),
                                as.factor))
  
  # dataset <- dplyr::rename(dataset, Period_var_temp = period_variable)
  # dataset$Period_var_temp <- as.character(dataset$Period_var_temp)
  #
  # # Data processing categorical variables
  # dataset <- dplyr::mutate(dataset, dplyr::across(dplyr::all_of(categorical_variables),
  #                                        as.factor)) # Transform columns to factor
  
  # Create list of periods
  period_list <- sort(unique(dataset$period_var_temp), decreasing = FALSE)
  
  # Calculate laspeyres imputations and numbers
  tbl_average_imputation <-
    calculate_hedonic_imputation(dataset_temp = dataset
                                 , period_temp = "period_var_temp"
                                 , dependent_variable_temp = dependent_variable
                                 , independent_variables_temp = independent_variables
                                 , log_dependent_temp = log_dependent
                                 , number_of_observations_temp = number_of_observations
                                 , period_list_temp = period_list)
  
  
  # Calculate index
  Index <- calculate_index(tbl_average_imputation$period, tbl_average_imputation$average_imputation, reference_period = reference_period)
  
  # Create table
  laspeyres <- data.frame(period = tbl_average_imputation$period)
  
  if (number_of_observations == TRUE) {
    laspeyres$number_of_observations <- tbl_average_imputation$number_of_observations
  }
  if (imputation == TRUE) {
    laspeyres$Imputation <- tbl_average_imputation$average_imputation
  }
  if (index == TRUE) {
    laspeyres$Index <- Index
  }
 
  
  return(laspeyres)
  
}
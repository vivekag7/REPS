
#' Calculate direct index according to the Fisher hedonic double imputation method
#'
#' By the parameters 'dependent_variable', 'continue_variable' and 'categorical_variables' as regression model is compiled.
#' With the model, a direct series of index figures is estimated by use of hedonic regression.
#'
#' N.B.: the independent variables must be entered transformed (and ready) in the parameters.
#' Hence, not: log(floor_area), but transform the variable in advance and then provide log_floor_area.
#' This does not count for the dependent variable. This should be entered untransformed/
#' The parameter log_dependent can be used to transform this variable.
#'
#' Within the data, it is not neccesary to filter the data on relevant variables or complete records.
#' This is taken care of in the function.
#'
#' @author Farley Ishaak (FIHK)
#' @param dataset table with data (does not need to be a selection of relevant variables)
#' @param period_variable variable in the table with periods
#' @param dependent_variable usually the sale price
#' @param continious_variables vector with quality determining numeric variables (no dummies)
#' @param categorical_variables vector with quality determining categorical variables (also dummies)
#' @param log_dependent should the dependent variable be transformed to its logarithm? default = TRUE
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period
#' @export
#' @examples
#'
#' Tbl_Fisher <- calculate_fisher(dataset = data_constraxion
#'                                 , period_variable = c("period")
#'                                 , dependent_variable = c('price')
#'                                 , continious_variables = c('floor_area')
#'                                 , categorical_variables = c('neighbourhood_code')
#'                                 , log_dependent = TRUE
#'                                 , reference_period = 2015
#'                                 , number_of_observations = TRUE)


calculate_fisher <- function(dataset
                             , period_variable
                             , dependent_variable
                             , continious_variables
                             , categorical_variables
                             , log_dependent = FALSE
                             , reference_period = NULL
                             , number_of_observations = FALSE) {

  # Calculate Laspeyres with 1th period = 100
  laspeyres <- calculate_laspeyres(dataset = dataset
                                   , period_variable = period_variable
                                   , dependent_variable = dependent_variable
                                   , continious_variables = continious_variables
                                   , categorical_variables = categorical_variables
                                   , log_dependent = log_dependent
                                   , reference_period = NULL
                                   , index = TRUE
                                   , number_of_observations = number_of_observations
                                   , imputation = FALSE)

  # Calculate Paasche with 1th period = 100
  paasche <- calculate_paasche(dataset = dataset
                               , period_variable = period_variable
                               , dependent_variable = dependent_variable
                               , continious_variables = continious_variables
                               , categorical_variables = categorical_variables
                               , log_dependent = log_dependent
                               , reference_period = NULL
                               , index = TRUE
                               , number_of_observations = number_of_observations
                               , imputation = FALSE)

  # Calculate Fisher (= geometric average)
  Index <- sqrt(laspeyres$Index * paasche$Index)

  # Rescale to reference_year
  Index <- calculate_index(laspeyres$period, Index, reference_period)

  # Create table
  fisher <- data.frame(period = laspeyres$period)

  if (number_of_observations == TRUE) {
    fisher$number_of_observations <- laspeyres$number_of_observations
  }

  fisher$Index <- Index

  return(fisher)

}

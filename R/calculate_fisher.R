#' Calculate direct index according to the Fisher hedonic double imputation method
#'
#' By the parameters 'dependent_variable', 'continue_variable' and 'categorical_variables' as regression model is compiled.
#' With the model, a direct series of index figures is estimated by use of hedonic regression.
#'
#' N.B.: the independent variables must be entered transformed (and ready) in the parameters.
#' Hence, not: log(floor_area), but transform the variable in advance and then provide log_floor_area.
#' This does not count for the dependent variable. This should be entered untransformed
#'
#' Within the data, it is not neccesary to filter the data on relevant variables or complete records.
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
#' @param parallel Logical; whether independent Laspeyres and Paasche calculations are parallelized.
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period
#' @keywords internal
#' @noRd
calculate_fisher <- function(dataset
                             , period_variable
                             , dependent_variable
                             , numerical_variables
                             , categorical_variables
                             , reference_period = NULL
                             , number_of_observations = FALSE
                             , parallel = FALSE) {

  calculate_fisher_component <- function(component) {
    if (component == "laspeyres") {
      return(calculate_laspeyres(dataset = dataset
                                 , period_variable = period_variable
                                 , dependent_variable = dependent_variable
                                 , numerical_variables = numerical_variables
                                 , categorical_variables = categorical_variables
                                 , reference_period = NULL
                                 , number_of_observations = number_of_observations
                                 , imputation = FALSE))
    }

    calculate_paasche(dataset = dataset
                      , period_variable = period_variable
                      , dependent_variable = dependent_variable
                      , numerical_variables = numerical_variables
                      , categorical_variables = categorical_variables
                      , reference_period = NULL
                      , number_of_observations = number_of_observations
                      , imputation = FALSE
                      , parallel = FALSE)
  }

  component_results <- run_parallel_tasks(
    tasks = c("laspeyres", "paasche"),
    task_function = calculate_fisher_component,
    parallel = parallel,
    fallback_message = "Parallel Fisher calculation failed; falling back to sequential calculation."
  )
  names(component_results) <- c("laspeyres", "paasche")

  laspeyres <- component_results$laspeyres
  paasche <- component_results$paasche

  # Calculate Fisher (= geometric average)
  Index <- sqrt(laspeyres$Index * paasche$Index)

  # FORMAT OUTPUT
  obs_counts <- if (number_of_observations) laspeyres$number_of_observations else NULL
  
  fisher <- format_index_output(
    periods = laspeyres$period,
    index_values = Index,
    reference_period = reference_period,
    observation_counts = obs_counts
  )

  return(fisher)
}

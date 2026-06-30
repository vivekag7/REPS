#' Calculate index based on specified method (Fisher, Laspeyres, Paasche, HMTS, Time Dummy, Rolling Time Dummy)
#'
#' Central hub function to calculate index figures using different methods. Can also calculate chained indices using the Annual Overlap Method.
#'
#' @author Vivek Gajadhar
#' @param method One of: "fisher", "laspeyres", "paasche", "hmts", "timedummy", "rolling_timedummy", "repricing"
#' @param dataset Data frame with input data
#' @param period_variable A string with the name of the column containing time periods.
#' @param dependent_variable Usually the price
#' @param numerical_variables Vector with numeric quality-determining variables
#' @param categorical_variables Vector with categorical variables (also dummies)
#' @param reference_period Period or group of periods that will be set to 100
#' @param number_of_observations Logical, whether to show number of observations (default = TRUE)
#' @param chained Logical. If TRUE, calculates a chained index using the Annual Overlap Method. Default is FALSE.
#' @param index_contribution Logical. If TRUE, calculates an index-contribution table for one method. Default is FALSE.
#' @param parallel Logical. If TRUE, independent calculations are parallelized where useful. Default is FALSE.
#' @param ... Additional method-specific arguments passed to the underlying functions:
#' \itemize{
#'   \item \code{periods_in_year}: (Required for Repricing) Number of periods per year (e.g. 12 for months, 4 for quarters)
#'   \item \code{number_preliminary_periods}: (Optional for HMTS) Number of preliminary periods. Default = 3
#'   \item \code{production_since}: (Optional for HMTS) Start period for production simulation. Default = NULL
#'   \item \code{resting_points}: (Optional for HMTS) Whether to return detailed outputs. Default = FALSE
#'   \item \code{imputation}: (Optional for Laspeyres/Paasche) Include imputation values? Default = FALSE
#'   \item \code{window_length}: (Optional for Rolling Time Dummy) Window size in number of periods. Default = 5
#'   \item \code{unit_variable}: (Optional for index contribution) Unit column to exclude as groups. Default = NULL
#'   \item \code{index_contribution_period}: (Optional for index contribution) Period to analyze. Default = latest period
#' }
#'
#' @return A data.frame (or list for HMTS with resting_points = TRUE; named list if multiple methods are used; or list with Index and Index_contribution when index_contribution = TRUE)
#' @examples
#' \dontrun{
#' data("hedonic_data")
#'
#' Tbl_indices <- REPS::calculate_hedonic_index(
#'   method = c("fisher", "hmts", "laspeyres", "paasche",
#'  "repricing", "timedummy", "rolling_timedummy"),
#'   dataset = hedonic_data,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   numerical_variables = c("floor_area", "dist_trainstation"),
#'   categorical_variables = c("neighbourhood_code", "dummy_large_city"),
#'   reference_period = "2015",
#'   number_of_observations = FALSE,
#'   periods_in_year = 4,
#'   number_preliminary_periods = 1,
#'   window_length = 4,
#'   production_since = NULL,
#'   resting_points = FALSE,
#'   imputation = FALSE
#' )
#' }
#' @export
calculate_hedonic_index <- function(dataset,
                                    method,
                                    period_variable,
                                    dependent_variable,
                                    numerical_variables = NULL,
                                    categorical_variables = NULL,
                                    reference_period = NULL,
                                    number_of_observations = TRUE,
                                    chained = FALSE,
                                    index_contribution = FALSE,
                                    parallel = FALSE,
                                    ...) {
  method <- validate_hedonic_index_methods(method)
  extra_args <- list(...)
  extra_args$parallel <- parallel

  validate_hedonic_index_options(
    method = method,
    chained = chained,
    index_contribution = index_contribution,
    extra_args = extra_args
  )

  validate_input(
    dataset,
    period_variable,
    dependent_variable,
    numerical_variables,
    categorical_variables
  )

  make_run_method <- function(method_extra_args) {
    function(method_name, target_dataset, target_reference_period) {
      run_hedonic_index_method(
        method = method_name,
        dataset = target_dataset,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        numerical_variables = numerical_variables,
        categorical_variables = categorical_variables,
        reference_period = target_reference_period,
        number_of_observations = number_of_observations,
        extra_args = method_extra_args
      )
    }
  }

  run_method <- make_run_method(extra_args)
  method_calculation_args <- extra_args
  if (isTRUE(parallel) && length(method) > 1) {
    method_calculation_args$parallel <- FALSE
  }
  run_method_for_method_calculation <- make_run_method(method_calculation_args)

  calculate_method_result <- function(method_name) {
    calculate_single_hedonic_index_method(
      method = method_name,
      dataset = dataset,
      period_variable = period_variable,
      reference_period = reference_period,
      chained = chained,
      run_method = run_method_for_method_calculation
    )
  }

  result <- run_parallel_tasks(
    tasks = method,
    task_function = calculate_method_result,
    parallel = isTRUE(parallel) && length(method) > 1,
    fallback_message = "Parallel method calculation failed; falling back to sequential calculation."
  )

  if (length(method) == 1) {
    if (isTRUE(index_contribution)) {
      index_contribution_extra_args <- extra_args
      index_contribution_extra_args$parallel <- FALSE

      run_index_contribution_method <- function(method_name, target_dataset, target_reference_period) {
        run_hedonic_index_method(
          method = method_name,
          dataset = target_dataset,
          period_variable = period_variable,
          dependent_variable = dependent_variable,
          numerical_variables = numerical_variables,
          categorical_variables = categorical_variables,
          reference_period = target_reference_period,
          number_of_observations = number_of_observations,
          extra_args = index_contribution_extra_args
        )
      }

      contribution_result <- calculate_index_contribution(
        dataset = dataset,
        index_output = result[[1]],
        period_variable = period_variable,
        calculate_index_function = function(target_dataset) {
          calculate_single_hedonic_index_method(
            method = method,
            dataset = target_dataset,
            period_variable = period_variable,
            reference_period = reference_period,
            chained = chained,
            run_method = run_index_contribution_method
          )
        },
        unit_variable = extra_args$unit_variable,
        index_contribution_period = extra_args$index_contribution_period,
        parallel = parallel
      )

      return(list(
        Index = result[[1]],
        Index_contribution = contribution_result
      ))
    }

    return(result[[1]])
  }

  names(result) <- method
  result
}

#' Supported Hedonic Index Methods
#'
#' Returns the canonical method names accepted by `calculate_hedonic_index()`.
#'
#' @return Character vector with supported method names.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
supported_hedonic_index_methods <- function() {
  c(
    "fisher",
    "laspeyres",
    "paasche",
    "hmts",
    "timedummy",
    "rolling_timedummy",
    "repricing"
  )
}

#' Hedonic Index Method Function Map
#'
#' Maps each public method name to the internal function that performs the
#' actual calculation.
#'
#' @return Named character vector of method names and function names.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
hedonic_index_method_functions <- function() {
  c(
    fisher = "calculate_fisher",
    laspeyres = "calculate_laspeyres",
    paasche = "calculate_paasche",
    hmts = "calculate_hmts",
    timedummy = "calculate_time_dummy",
    rolling_timedummy = "calculate_rolling_timedummy",
    repricing = "calculate_repricing"
  )
}

#' Validate Hedonic Index Methods
#'
#' Normalizes method names to lower case and stops when one or more requested
#' methods are not supported by `calculate_hedonic_index()`.
#'
#' @param method Character vector of requested method names.
#' @return Character vector of normalized method names.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_hedonic_index_methods <- function(method) {
  method <- tolower(method)
  valid_methods <- supported_hedonic_index_methods()
  invalid_methods <- setdiff(method, valid_methods)

  if (length(invalid_methods) > 0) {
    stop(paste0(
      "Invalid method(s): ",
      paste(invalid_methods, collapse = ", "),
      ". Please choose from: ",
      paste(valid_methods, collapse = ", "),
      "."
    ))
  }

  method
}

#' Validate Hedonic Index Option Combinations
#'
#' Checks cross-method options that cannot be validated by an individual method,
#' such as using HMTS resting points in multi-method or chained calculations.
#'
#' @param method Character vector of normalized method names.
#' @param chained Logical; whether annual-overlap chaining is requested.
#' @param index_contribution Logical; whether index-contribution output is requested.
#' @param extra_args List of method-specific arguments supplied through `...`.
#' @return Invisibly returns `TRUE` when the option combination is valid.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_hedonic_index_options <- function(method, chained, index_contribution, extra_args) {
  if (length(method) > 1 && isTRUE(extra_args$resting_points)) {
    stop("Using 'resting_points = TRUE' is only allowed with a single method ('hmts').")
  }

  if (length(method) > 1 && isTRUE(index_contribution)) {
    stop("Using 'index_contribution = TRUE' is only allowed with a single method.")
  }

  if (isTRUE(chained) && isTRUE(extra_args$resting_points)) {
    stop("Using 'chained = TRUE' together with 'resting_points = TRUE' is not supported, because chained calculations require a regular index data.frame.")
  }

  if (isTRUE(index_contribution) && isTRUE(extra_args$resting_points)) {
    stop("Using 'index_contribution = TRUE' together with 'resting_points = TRUE' is not supported, because index contribution requires a regular index data.frame.")
  }

  invisible(TRUE)
}

#' Get Hedonic Index Method Function
#'
#' Resolves a normalized method name to the internal calculation function used
#' by `calculate_hedonic_index()`.
#'
#' @param method Single normalized method name.
#' @return Function object for the requested method.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
get_hedonic_index_method_function <- function(method) {
  switch(
    method,
    fisher = calculate_fisher,
    laspeyres = calculate_laspeyres,
    paasche = calculate_paasche,
    hmts = calculate_hmts,
    timedummy = calculate_time_dummy,
    rolling_timedummy = calculate_rolling_timedummy,
    repricing = calculate_repricing,
    stop("Invalid method: ", method)
  )
}

#' Resolve Method-Specific Extra Arguments
#'
#' Filters `...` arguments to those accepted by the target method and applies
#' central defaults for methods that need extra arguments.
#'
#' @param method Single normalized method name.
#' @param target_function Function object for the requested method.
#' @param extra_args List of method-specific arguments supplied through `...`.
#' @return List of arguments accepted by `target_function`, including defaults.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
resolve_hedonic_method_extra_args <- function(method, target_function, extra_args) {
  accepted_args <- names(formals(target_function))
  valid_extra_args <- extra_args[names(extra_args) %in% accepted_args]

  if (method == "rolling_timedummy" && !("window_length" %in% names(valid_extra_args))) {
    valid_extra_args$window_length <- 5
    message("Note: 'window_length' was not specified. A default value of 5 has been applied.")
  }

  if (method == "hmts") {
    if (!("number_preliminary_periods" %in% names(valid_extra_args))) {
      valid_extra_args$number_preliminary_periods <- 3
      message("Note: 'number_preliminary_periods' was not specified. A default value of 3 has been applied.")
    }

    if (!("production_since" %in% names(valid_extra_args))) {
      valid_extra_args$production_since <- NULL
      message("Note: 'production since' was not specified. A default value of NULL has been applied. Enter the initial production period to establish a definitive timeline for all future calculations.")
    }

    if (!("resting_points" %in% names(valid_extra_args))) {
      valid_extra_args$resting_points <- FALSE
    }
  }

  if (method == "repricing" && !("periods_in_year" %in% names(valid_extra_args))) {
    stop("Validation Error: You must specify 'periods_in_year' for the 'repricing' method.")
  }

  if (method %in% c("laspeyres", "paasche") && !("imputation" %in% names(valid_extra_args))) {
    valid_extra_args$imputation <- FALSE
  }

  valid_extra_args
}

#' Run a Hedonic Index Method
#'
#' Builds the shared argument list for a hedonic index method, resolves
#' method-specific arguments, and calls the target calculation function.
#'
#' @param method Single normalized method name.
#' @param dataset Data frame with input data.
#' @param period_variable Name of the period column.
#' @param dependent_variable Name of the dependent variable, usually price.
#' @param numerical_variables Character vector of numerical quality variables.
#' @param categorical_variables Character vector of categorical quality variables.
#' @param reference_period Optional reference period used to normalize the index.
#' @param number_of_observations Logical; whether observation counts are returned.
#' @param extra_args List of method-specific arguments supplied through `...`.
#' @return Method-specific index output.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
run_hedonic_index_method <- function(method,
                                     dataset,
                                     period_variable,
                                     dependent_variable,
                                     numerical_variables,
                                     categorical_variables,
                                     reference_period,
                                     number_of_observations,
                                     extra_args) {
  target_function <- get_hedonic_index_method_function(method)
  valid_extra_args <- resolve_hedonic_method_extra_args(
    method = method,
    target_function = target_function,
    extra_args = extra_args
  )

  base_args <- list(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    numerical_variables = numerical_variables,
    categorical_variables = categorical_variables,
    reference_period = reference_period,
    number_of_observations = number_of_observations
  )

  do.call(target_function, c(base_args, valid_extra_args))
}

#' Calculate One Hedonic Index Method
#'
#' Routes a single requested method to either the direct method calculation or
#' the annual-overlap chained calculation.
#'
#' @param method Single normalized method name.
#' @param dataset Data frame with input data.
#' @param period_variable Name of the period column.
#' @param reference_period Optional reference period used to normalize the index.
#' @param chained Logical; whether annual-overlap chaining is requested.
#' @param run_method Callback used to run one method on a target dataset.
#' @return Data frame or method-specific output for one index method.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
calculate_single_hedonic_index_method <- function(method,
                                                  dataset,
                                                  period_variable,
                                                  reference_period,
                                                  chained,
                                                  run_method) {
  if (!isTRUE(chained)) {
    return(run_method(method, dataset, reference_period))
  }

  calculate_chained_hedonic_index(
    dataset = dataset,
    period_variable = period_variable,
    reference_period = reference_period,
    run_method = function(target_dataset, target_reference_period) {
      run_method(method, target_dataset, target_reference_period)
    }
  )
}

#' Extract Years From Period Labels
#'
#' Extracts the first four characters from period labels and converts them to
#' integer years for annual-overlap chaining.
#'
#' @param periods Character vector of period labels.
#' @return Integer vector of years; invalid period labels become `NA`.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
get_hedonic_period_years <- function(periods) {
  suppressWarnings(as.integer(substr(periods, 1, 4)))
}

#' Validate Chained Index Method Output
#'
#' Ensures that a method result can be used by the annual-overlap chaining
#' logic, which requires `period` and `Index` columns.
#'
#' @param index_result Object returned by a hedonic index method.
#' @return Invisibly returns `TRUE` when the result has the required structure.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_chained_index_result <- function(index_result) {
  if (!is.data.frame(index_result) ||
      !("period" %in% names(index_result)) ||
      !("Index" %in% names(index_result))) {
    stop("Chained index calculation requires each method to return a data.frame with columns 'period' and 'Index'.")
  }

  invisible(TRUE)
}

#' Calculate Annual-Overlap Index Segments
#'
#' Splits the dataset into yearly calculation windows. The first year is
#' calculated on its own, and later years include the final period of the
#' previous year as the overlap period.
#'
#' @param dataset Data frame with input data.
#' @param period_variable Name of the period column.
#' @param unique_periods Sorted character vector of unique period labels.
#' @param period_years Integer vector of years aligned with `unique_periods`.
#' @param years Integer vector of years to calculate.
#' @param run_method Callback that calculates an index for one dataset segment.
#' @return Named list of short-term index data frames by year.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
calculate_annual_overlap_segments <- function(dataset,
                                              period_variable,
                                              unique_periods,
                                              period_years,
                                              years,
                                              run_method) {
  short_term_results <- list()
  first_year <- min(years)
  first_year_periods <- unique_periods[period_years == first_year]
  data_subset_first <- dataset[dataset[[period_variable]] %in% first_year_periods, ]

  short_term_results[[as.character(first_year)]] <- run_method(data_subset_first, NULL)
  validate_chained_index_result(short_term_results[[as.character(first_year)]])

  if (length(years) > 1) {
    for (i in 2:length(years)) {
      current_year <- years[i]
      prev_year <- years[i - 1]

      current_periods <- unique_periods[period_years == current_year]
      prev_periods <- unique_periods[period_years == prev_year]
      overlap_period <- utils::tail(sort(prev_periods), 1)

      calculation_periods <- c(overlap_period, current_periods)
      data_subset <- dataset[dataset[[period_variable]] %in% calculation_periods, ]
      index_current <- run_method(data_subset, overlap_period)

      validate_chained_index_result(index_current)

      short_term_results[[as.character(current_year)]] <-
        index_current[index_current$period != overlap_period, ]
    }
  }

  short_term_results
}

#' Splice Annual-Overlap Index Segments
#'
#' Combines short-term annual-overlap index segments into one chained index
#' series by multiplying later-year growth factors onto the previous level.
#'
#' @param short_term_results Named list of segment-level index data frames.
#' @return Data frame with `period` and chained `Index` columns.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
#' @importFrom dplyr bind_rows
splice_annual_overlap_index <- function(short_term_results) {
  full_series <- dplyr::bind_rows(short_term_results)
  full_series <- full_series[order(full_series$period), ]

  final_index <- numeric(nrow(full_series))
  periods_vec <- full_series$period
  n_y1 <- nrow(short_term_results[[1]])

  if (n_y1 == 0) {
    stop("The first year does not contain enough observations to calculate a chained index.")
  }

  final_index[1:n_y1] <-
    short_term_results[[1]]$Index /
    short_term_results[[1]]$Index[1] *
    100

  current_idx <- n_y1 + 1

  for (i in seq_along(short_term_results)[-1]) {
    factors <- short_term_results[[i]]$Index / 100
    n_obs <- length(factors)

    if (n_obs == 0) {
      next
    }

    previous_level <- final_index[current_idx - 1]
    final_index[current_idx:(current_idx + n_obs - 1)] <- factors * previous_level
    current_idx <- current_idx + n_obs
  }

  data.frame(
    period = periods_vec,
    Index = final_index
  )
}

#' Calculate Chained Hedonic Index
#'
#' Coordinates annual-overlap chaining for one hedonic index method and rebases
#' the resulting series when a reference period is provided.
#'
#' @param dataset Data frame with input data.
#' @param period_variable Name of the period column.
#' @param reference_period Optional reference period used to normalize the index.
#' @param run_method Callback that calculates one method on a target dataset.
#' @return Data frame with `period` and chained `Index` columns.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
calculate_chained_hedonic_index <- function(dataset,
                                            period_variable,
                                            reference_period,
                                            run_method) {
  periods_raw <- as.character(dataset[[period_variable]])
  unique_periods <- sort(unique(periods_raw))
  period_years <- get_hedonic_period_years(unique_periods)
  years <- unique(period_years)

  if (any(is.na(years))) {
    stop("Chained index calculation requires period values where the first four characters represent the year, for example '2015Q1', '2015-01', or '201501'.")
  }

  short_term_results <- calculate_annual_overlap_segments(
    dataset = dataset,
    period_variable = period_variable,
    unique_periods = unique_periods,
    period_years = period_years,
    years = years,
    run_method = run_method
  )

  result_table <- splice_annual_overlap_index(short_term_results)

  if (!is.null(reference_period)) {
    result_table$Index <- calculate_index(
      result_table$period,
      result_table$Index,
      reference_period
    )
  }

  result_table
}


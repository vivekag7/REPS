#' Calculate a sales price appraisal ratio index
#'
#' Central hub function for calculating sales price appraisal ratio (SPAR)
#' indices. The arithmetic, geometric, and unweighted variants are supported.
#' Multiple methods can be calculated in one call, and short-term SPAR series
#' can optionally be chained.
#'
#' @author Kai Cheung, Vivek Gajadhar
#' @param dataset A data frame containing the input data.
#' @param method One or more of `"arithmetic"`, `"geometric"`, and
#'   `"unweighted"`.
#' @param period_variable A string naming the period column.
#' @param dependent_variable A string naming the transaction price column.
#' @param appraisal_variable A string naming the appraisal value column.
#' @param grouping_variables A character vector naming the variables that
#'   identify each short-term SPAR series, for example appraisal year and
#'   building type.
#' @param index_type_variable A character vector naming the variables that
#'   identify independent series when `chained = TRUE`, for example building
#'   type. Each variable must also occur in `grouping_variables`. May be `NULL`
#'   when `chained = FALSE`.
#' @param reference_period_pattern A regular expression identifying the
#'   reference period within each group. The default matches periods ending in
#'   `"01"`.
#' @param chained Logical; whether to chain the short-term indices. The default
#'   is `FALSE`.
#' @param base_value Positive numeric value assigned to the beginning of each
#'   chained series. The default is 100.
#' @param month_modulo Positive number used to identify the first period of a
#'   new chain segment. For periods such as `202001`, the default of 100 treats
#'   values with remainder 1 as segment starts.
#' @param parallel Logical; whether independent method calculations may be run
#'   in parallel. The default is `FALSE`.
#'
#' @return A data frame for one method. When multiple methods are requested, a
#'   named list of data frames is returned, matching `calculate_hedonic_index()`.
#'   Every result contains standardized `period` and `Index` columns for
#'   compatibility with `plot_price_index()`. Groups without a matching
#'   reference period are omitted with a warning.
#'
#' @examples
#' spar_data <- data.frame(
#'   period = rep(c(202301, 202302), each = 2),
#'   appraisal_year = 2023,
#'   property_type = rep(c("apartment", "house"), 2),
#'   price = c(200, 300, 220, 330),
#'   appraisal = c(180, 270, 180, 270)
#' )
#'
#' # Valid methods are "arithmetic", "geometric", and "unweighted".
#' # A single method returns one data frame.
#' single_result <- calculate_spar(
#'   dataset = spar_data,
#'   method = "arithmetic",
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   appraisal_variable = "appraisal",
#'   grouping_variables = c("appraisal_year", "property_type")
#' )
#' head(single_result)
#'
#' # Multiple methods return a named list, as in calculate_hedonic_index().
#' multiple_result <- calculate_spar(
#'   dataset = spar_data,
#'   method = c("arithmetic", "geometric", "unweighted"),
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   appraisal_variable = "appraisal",
#'   grouping_variables = c("appraisal_year", "property_type")
#' )
#' names(multiple_result)
#'
#' @export
calculate_spar <- function(dataset,
                           method,
                           period_variable,
                           dependent_variable,
                           appraisal_variable,
                           grouping_variables,
                           index_type_variable = NULL,
                           reference_period_pattern = "01$",
                           chained = FALSE,
                           base_value = 100,
                           month_modulo = 100,
                           parallel = FALSE) {
  method <- validate_spar_index_methods(method)

  validate_spar_index_options(
    grouping_variables = grouping_variables,
    index_type_variable = index_type_variable,
    chained = chained,
    base_value = base_value,
    month_modulo = month_modulo,
    parallel = parallel
  )

  validate_spar_input(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    appraisal_variable = appraisal_variable,
    grouping_variables = grouping_variables,
    reference_period_pattern = reference_period_pattern
  )

  calculate_method_result <- function(method_name) {
    calculate_single_spar_index_method(
      method = method_name,
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      appraisal_variable = appraisal_variable,
      grouping_variables = grouping_variables,
      index_type_variable = index_type_variable,
      reference_period_pattern = reference_period_pattern,
      chained = chained,
      base_value = base_value,
      month_modulo = month_modulo
    )
  }

  result <- run_parallel_tasks(
    tasks = method,
    task_function = calculate_method_result,
    parallel = isTRUE(parallel) && length(method) > 1L,
    fallback_message = paste0(
      "Parallel SPAR method calculation failed; ",
      "falling back to sequential calculation."
    )
  )

  if (length(method) == 1L) {
    return(result[[1L]])
  }

  names(result) <- method
  result
}

#' Supported SPAR index methods
#'
#' @return A character vector of canonical method names.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
supported_spar_index_methods <- function() {
  c("arithmetic", "geometric", "unweighted")
}

#' Validate SPAR index methods
#'
#' @param method Character vector of requested methods.
#' @return A character vector of normalized method names.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_spar_index_methods <- function(method) {
  if (!is.character(method) || length(method) == 0L || anyNA(method)) {
    stop("`method` must contain one or more method names.", call. = FALSE)
  }

  method <- tolower(method)
  method <- unique(method)

  invalid_methods <- setdiff(method, supported_spar_index_methods())
  if (length(invalid_methods) > 0L) {
    stop(
      "Invalid method(s): ", paste(invalid_methods, collapse = ", "),
      ". Please choose from: ",
      paste(supported_spar_index_methods(), collapse = ", "), ".",
      call. = FALSE
    )
  }

  method
}

#' Validate SPAR hub options
#'
#' @param grouping_variables Character vector of grouping columns.
#' @param index_type_variable Character vector of chain-series columns.
#' @param chained Logical chain option.
#' @param base_value Chained index base value.
#' @param month_modulo Chained segment modulus.
#' @param parallel Logical parallel option.
#' @return Invisibly returns `TRUE`.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_spar_index_options <- function(grouping_variables,
                                        index_type_variable,
                                        chained,
                                        base_value,
                                        month_modulo,
                                        parallel) {
  validate_scalar_logical(chained, "chained")
  validate_scalar_logical(parallel, "parallel")

  if (!isTRUE(chained)) {
    return(invisible(TRUE))
  }

  validate_spar_column_names(index_type_variable, "index_type_variable")
  missing_grouping_variables <- setdiff(index_type_variable, grouping_variables)
  if (length(missing_grouping_variables) > 0L) {
    stop(
      "Every `index_type_variable` must also occur in `grouping_variables`.",
      call. = FALSE
    )
  }
  validate_positive_scalar(base_value, "base_value")
  validate_positive_scalar(month_modulo, "month_modulo")

  invisible(TRUE)
}

#' Calculate one SPAR index method
#'
#' @inheritParams calculate_spar
#' @return A data frame containing one SPAR result.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
calculate_single_spar_index_method <- function(method,
                                               dataset,
                                               period_variable,
                                               dependent_variable,
                                               appraisal_variable,
                                               grouping_variables,
                                               index_type_variable,
                                               reference_period_pattern,
                                               chained,
                                               base_value,
                                               month_modulo) {
  result <- run_spar_index_method(
    method = method,
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    appraisal_variable = appraisal_variable,
    grouping_variables = grouping_variables,
    reference_period_pattern = reference_period_pattern
  )

  if (isTRUE(chained)) {
    result <- chain_spar_index(
      df = result,
      period_variable = period_variable,
      index_type_variable = index_type_variable,
      growth_variable = "growth_rate",
      base_value = base_value,
      month_modulo = month_modulo
    )
  } else {
    result$Index <- 100 * result$growth_rate
  }

  result[[period_variable]] <- as.character(result[[period_variable]])
  names(result)[names(result) == period_variable] <- "period"
  result
}

#' Run a SPAR index method
#'
#' @inheritParams calculate_spar
#' @return A data frame containing a short-term SPAR result.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
run_spar_index_method <- function(method,
                                  dataset,
                                  period_variable,
                                  dependent_variable,
                                  appraisal_variable,
                                  grouping_variables,
                                  reference_period_pattern) {
  target_function <- get_spar_index_method_function(method)
  target_function(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    appraisal_variable = appraisal_variable,
    grouping_variables = grouping_variables,
    reference_period_pattern = reference_period_pattern
  )
}

#' Get a SPAR index method function
#'
#' @param method A canonical SPAR method name.
#' @return The internal calculation function for `method`.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
get_spar_index_method_function <- function(method) {
  switch(
    method,
    arithmetic = calculate_spar_arithmetic,
    geometric = calculate_spar_geometric,
    unweighted = calculate_spar_unweighted,
    stop("Invalid method: ", method, call. = FALSE)
  )
}

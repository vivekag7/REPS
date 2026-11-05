#' Calculate a sales price appraisal ratio index
#'
#' Calculates arithmetic, geometric, or unweighted sales price appraisal ratio
#' (SPAR) indices from transaction prices and appraisal values. Each
#' `grouping_variables` combination defines a short-term series, normalized by
#' its reference-period ratio. No hedonic regression is fitted.
#'
#' @details
#' Arithmetic SPAR uses `mean(price) / mean(appraisal)`, geometric SPAR uses
#' `exp(mean(log(price))) / exp(mean(log(appraisal)))`, and unweighted SPAR uses
#' `mean(price / appraisal)`. Following the reference script, arithmetic and
#' geometric aggregates remove missing prices and appraisals separately;
#' unweighted removes missing individual ratios. A period/group without enough
#' observed values for its method is an error. Missing or
#' blank period/group identifiers are errors. Groups without a reference period
#' are warned about and omitted; multiple matching periods in a group are errors.
#'
#' Without chaining, `Index = 100 * growth_rate`, where `growth_rate` is the
#' period ratio divided by its group's reference ratio. With chaining, each
#' independent series begins at `base_value`. Its first `growth_rate` is 1;
#' at later reference periods it is the current reference ratio divided by the
#' previous row's reference ratio. Other periods use the short-term ratio.
#' `growth_rate_long` updates at the first row and periods ending in `"12"`,
#' and is then carried forward. `yoy_change` equals that rate at reference
#' periods and 1 elsewhere; `chain_long = cumprod(yoy_change)`.
#' The final `Index = base_value * growth_rate * chain_long` reproduces the
#' supplied SPAR script's `index_long`, including reference-period jumps and
#' subsequent returns to the within-vintage development.
#'
#' Chained SPAR follows the validated monthly January/December methodology
#' from the reference implementation. Periods must be positive, six-digit
#' YYYYMM integers or numeric-like strings, with months 01 through 12.
#' Each grouping series occupies one calendar year and starts at its January
#' reference period; `reference_period_pattern = "01$"` is the usual setting.
#' The December rule is literal, not an inferred last available period.
#' Unchained SPAR also supports other labels, such as quarterly strings.
#'
#' @param dataset A non-empty data frame of transaction observations.
#' @param method One or more of `"arithmetic"`, `"geometric"`, `"unweighted"`.
#'   Names are case-insensitive; duplicates are calculated once.
#' @param period_variable One column name identifying periods.
#' @param dependent_variable One column name containing untransformed prices.
#' @param appraisal_variable One column name containing appraisal values.
#' @param grouping_variables Unique column names identifying short-term series,
#'   for example appraisal vintage and property type. These must be distinct
#'   from the period, price, and appraisal columns and from output column names.
#' @param index_type_variable Column names identifying independent chained
#'   series. Required for chaining and a subset of `grouping_variables`.
#'   Otherwise may be NULL.
#' @param reference_period_pattern One regular expression matching at most one
#'   period per group. Default `"01$"` matches January in `YYYYMM` periods.
#' @param chained Logical; link short-term series? Default FALSE.
#' @param base_value Positive finite starting level for chained series. Default
#'   100. Unchained indices always use 100.
#' @param parallel Logical; run multiple methods with the existing REPS parallel
#'   helper. Default FALSE.
#' @return One data frame, or a named list of data frames for multiple methods.
#'   Each contains character `period`, the grouping identifiers, ratios
#'   `ratio_numerator` and `ratio_denominator`, `growth_rate`, and `Index`.
#'   Arithmetic/geometric output also contains `numerator_numerator` and
#'   `numerator_denominator`. Chaining adds `chaining_true_false`,
#'   `growth_rate_long`, `yoy_change`, `chain_long`, and `growth_factor`
#'   (the actual period-on-period factor of `Index`). If all groups lack a
#'   reference, the result has zero rows.
#'   Filter grouped output to one series before using [plot_price_index()].
#' @author Kai Cheung, Vivek Gajadhar
#' @examples
#' data("arithmetic_data")
#' spar <- calculate_spar(
#'   arithmetic_data, c("arithmetic", "geometric", "unweighted"),
#'   period_variable = "period", dependent_variable = "price",
#'   appraisal_variable = "appraisal_value",
#'   grouping_variables = c("appraisal_year", "property_type"),
#'   index_type_variable = "property_type", reference_period_pattern = "01$",
#'   chained = TRUE
#' )
#' names(spar)
#' plot_price_index(lapply(spar, function(x) x[x$property_type == "Apartment", ]))
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
                           parallel = FALSE) {
  method <- validate_spar_index_methods(method)
  validate_spar_index_options(
    grouping_variables, index_type_variable, chained, base_value,
    parallel
  )
  validate_spar_input(
    dataset, period_variable, dependent_variable, appraisal_variable,
    grouping_variables
  )
  if (chained) validate_spar_monthly_periods(dataset[[period_variable]])
  dataset <- prepare_spar_data(
    dataset, period_variable, dependent_variable, appraisal_variable,
    grouping_variables, reference_period_pattern
  )
  if (chained) {
    validate_spar_chain_data(
      dataset, period_variable, grouping_variables,
      index_type_variable, reference_period_pattern
    )
  }

  result <- run_parallel_tasks(
    tasks = method,
    task_function = function(method_name) {
      calculate_single_spar_index_method(
        method_name, dataset, period_variable, dependent_variable,
        appraisal_variable, grouping_variables, index_type_variable,
        reference_period_pattern, chained, base_value
      )
    },
    parallel = parallel && length(method) > 1L,
    fallback_message = "Parallel SPAR method calculation failed; falling back to sequential calculation."
  )
  if (length(method) == 1L) {
    return(result[[1L]])
  }
  names(result) <- method
  result
}

#' @rdname calculate_spar-internal
#' @keywords internal
#' @noRd
supported_spar_index_methods <- function() {
  c("arithmetic", "geometric", "unweighted")
}

#' @rdname calculate_spar-internal
#' @keywords internal
#' @noRd
validate_spar_index_methods <- function(method) {
  if (!is.character(method) || length(method) == 0L || anyNA(method)) {
    stop("`method` must contain one or more method names.", call. = FALSE)
  }
  method <- unique(tolower(method))
  invalid <- setdiff(method, supported_spar_index_methods())
  if (length(invalid)) {
    stop(
      "Invalid method(s): ", paste(invalid, collapse = ", "),
      ". Please choose from: ",
      paste(supported_spar_index_methods(), collapse = ", "), ".",
      call. = FALSE
    )
  }
  method
}

#' @rdname calculate_spar-internal
#' @keywords internal
#' @noRd
get_spar_index_method_function <- function(method) {
  switch(
    method,
    arithmetic = aggregate_spar_arithmetic,
    geometric = aggregate_spar_geometric,
    unweighted = aggregate_spar_unweighted,
    stop("Invalid method: ", method, call. = FALSE)
  )
}

#' @rdname calculate_spar-internal
#' @keywords internal
#' @noRd
run_spar_index_method <- function(method, dataset, period_variable,
                                  dependent_variable, appraisal_variable,
                                  grouping_variables, reference_period_pattern) {
  calculate_spar_component(
    dataset, period_variable, dependent_variable, appraisal_variable,
    grouping_variables, reference_period_pattern,
    aggregate_function = get_spar_index_method_function(method),
    include_aggregates = method != "unweighted"
  )
}

#' @rdname calculate_spar-internal
#' @keywords internal
#' @noRd
calculate_single_spar_index_method <- function(method, dataset, period_variable,
                                               dependent_variable, appraisal_variable,
                                               grouping_variables, index_type_variable,
                                               reference_period_pattern, chained,
                                               base_value) {
  result <- run_spar_index_method(
    method, dataset, period_variable, dependent_variable,
    appraisal_variable, grouping_variables, reference_period_pattern
  )
  if (chained) {
    result <- chain_spar_index(
      result, period_variable, index_type_variable, base_value, reference_period_pattern
    )
  } else {
    result$Index <- 100 * result$growth_rate
  }
  if (any(!is.finite(result$Index) | result$Index <= 0)) {
    stop("SPAR output requires positive finite index levels.", call. = FALSE)
  }
  names(result)[names(result) == period_variable] <- "period"
  # A single formatting path retains diagnostics and all group identifiers.
  result <- as.data.frame(result)
  row.names(result) <- NULL
  result
}

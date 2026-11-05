#' Calculate a sales price appraisal ratio index
#'
#' Calculates arithmetic, geometric, or unweighted sales price appraisal ratio
#' (SPAR) indices from transaction prices and appraisal values. Each
#' `grouping_variables` combination defines a short-term series, normalized by
#' its reference-period ratio. No hedonic regression is fitted.
#' Optional `"median"` and `"mean"` methods provide unadjusted price baselines.
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
#' @param method One or more of `"spar_arithmetic"`, `"spar_geometric"`,
#'   `"spar_unweighted"`, `"median"`, or `"mean"`.
#'   Names are case-insensitive; duplicates are calculated once.
#' @param period_variable One column name identifying periods.
#' @param dependent_variable One column name containing untransformed prices.
#' @param appraisal_variable One column name containing appraisal values.
#'   Required for SPAR methods; unused by median and mean.
#' @param grouping_variables Unique column names identifying short-term series,
#'   for example appraisal vintage and property type. These must be distinct
#'   from the period, price, and appraisal columns and from output column names.
#'   Optional for baseline-only calls. Median and mean use `index_type_variable` when
#'   supplied, otherwise these groups, and normalize each series to its first
#'   usable period. They use no appraisal values or SPAR chaining formula.
#'   Mean averages untransformed prices within each period.
#' @param index_type_variable Column names identifying independent chained
#'   series. Required for chained SPAR and a subset of `grouping_variables`.
#'   Otherwise may be NULL.
#' @param reference_period_pattern One regular expression matching at most one
#'   period per group for SPAR. Default `"01$"` matches January in `YYYYMM` periods.
#' @param chained Logical; link short-term SPAR series? Default FALSE.
#'   Median and mean remain direct baselines.
#' @param base_value Positive finite starting level for chained series. Default
#'   100. Unchained indices always use 100.
#' @param parallel Logical; run multiple methods with the existing REPS parallel
#'   helper. Default FALSE.
#' @param index_contribution Logical; calculate leave-one-row/unit-out index and
#'   period-on-period contribution for a single method. Default FALSE.
#' @param unit_variable Optional column identifying units excluded together in
#'   the analyzed period. NULL excludes individual rows.
#' @param index_contribution_period Period to analyze, defaulting to the latest
#'   period in the index. Contributions are calculated separately per series.
#' @return One data frame, or a named list of data frames for multiple methods.
#'   SPAR results contain character `period`, the grouping identifiers, ratios
#'   `ratio_numerator` and `ratio_denominator`, `growth_rate`, and `Index`.
#'   Arithmetic/geometric output also contains `numerator_numerator` and
#'   `numerator_denominator`. Chaining adds `chaining_true_false`,
#'   `growth_rate_long`, `yoy_change`, `chain_long`, and `growth_factor`
#'   (the actual period-on-period factor of `Index`). If all groups lack a
#'   reference, the result has zero rows.
#'   With `index_contribution = TRUE`, returns a list containing `Index` and
#'   `Index_contribution`, including grouping identifiers. An exclusion that
#'   leaves the target index uncomputable has missing exclusion/difference values.
#'   Baseline results contain `period`, `Index`, and independent-series identifiers.
#'   Filter grouped output to one series before using [plot_price_index()].
#' @author Kai Cheung, Vivek Gajadhar
#' @examples
#' data("arithmetic_data")
#' spar <- calculate_spar_index(
#'   arithmetic_data, c("spar_arithmetic", "spar_geometric", "spar_unweighted"),
#'   period_variable = "period", dependent_variable = "price",
#'   appraisal_variable = "appraisal_value",
#'   grouping_variables = c("appraisal_year", "property_type"),
#'   index_type_variable = "property_type", reference_period_pattern = "01$",
#'   chained = TRUE
#' )
#' names(spar)
#' plot_price_index(lapply(spar, function(x) x[x$property_type == "Apartment", ]))
#' @export
calculate_spar_index <- function(dataset,
                           method,
                           period_variable,
                           dependent_variable,
                           appraisal_variable = NULL,
                           grouping_variables = NULL,
                           index_type_variable = NULL,
                           reference_period_pattern = "01$",
                           chained = FALSE,
                           base_value = 100,
                           parallel = FALSE,
                           index_contribution = FALSE,
                           unit_variable = NULL,
                           index_contribution_period = NULL) {
  method <- validate_spar_index_methods(method)
  baseline_methods <- c("median", "mean")
  has_spar <- any(!method %in% baseline_methods)
  validate_spar_index_options(
    grouping_variables, index_type_variable, chained, base_value,
    parallel, require_spar_groups = has_spar
  )
  baseline_groups <- if (!is.null(index_type_variable)) index_type_variable else grouping_variables
  if (has_spar) {
    validate_spar_input(dataset, period_variable, dependent_variable, appraisal_variable, grouping_variables)
  } else {
    validate_spar_baseline_input(dataset, period_variable, dependent_variable, baseline_groups)
  }
  validate_spar_contribution_options(
    method, dataset, index_contribution, unit_variable, index_contribution_period
  )
  input_dataset <- dataset
  if (has_spar && chained) validate_spar_monthly_periods(dataset[[period_variable]])
  if (has_spar) {
    dataset <- prepare_spar_data(
      dataset, period_variable, dependent_variable, appraisal_variable,
      grouping_variables, reference_period_pattern
    )
    if (chained) {
      validate_spar_chain_data(dataset, period_variable, grouping_variables,
        index_type_variable, reference_period_pattern)
    }
  }

  result <- run_parallel_tasks(
    tasks = method,
    task_function = function(method_name) {
      calculate_single_spar_index_method(
        method_name, if (method_name %in% baseline_methods) input_dataset else dataset,
        period_variable, dependent_variable,
        appraisal_variable, grouping_variables, index_type_variable,
        reference_period_pattern, chained, base_value
      )
    },
    parallel = parallel && length(method) > 1L,
    fallback_message = "Parallel SPAR method calculation failed; falling back to sequential calculation."
  )
  if (length(method) == 1L) {
    if (index_contribution) {
      recalculate <- function(target_dataset) {
        if (method %in% baseline_methods) {
          return(calculate_single_spar_index_method(
            method, target_dataset, period_variable, dependent_variable, appraisal_variable,
            grouping_variables, index_type_variable, reference_period_pattern, chained, base_value
          ))
        }
        prepared <- prepare_spar_data(
          target_dataset, period_variable, dependent_variable, appraisal_variable,
          grouping_variables, reference_period_pattern
        )
        calculate_single_spar_index_method(
          method, prepared, period_variable, dependent_variable, appraisal_variable,
          grouping_variables, index_type_variable, reference_period_pattern, chained, base_value
        )
      }
      contribution <- calculate_spar_index_contribution(
        input_dataset, result[[1L]], period_variable,
        if (method %in% baseline_methods) baseline_groups else grouping_variables,
        if (method %in% baseline_methods) baseline_groups else if (chained) index_type_variable else grouping_variables,
        recalculate, unit_variable, index_contribution_period, parallel
      )
      return(list(Index = result[[1L]], Index_contribution = contribution))
    }
    return(result[[1L]])
  }
  names(result) <- method
  result
}

#' @rdname calculate_spar_index-internal
#' @keywords internal
#' @noRd
supported_spar_index_methods <- function() {
  c("spar_arithmetic", "spar_geometric", "spar_unweighted", "median", "mean")
}

#' @rdname calculate_spar_index-internal
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

#' @rdname calculate_spar_index-internal
#' @keywords internal
#' @noRd
get_spar_index_method_function <- function(method) {
  switch(
    method,
    spar_arithmetic = aggregate_spar_arithmetic,
    spar_geometric = aggregate_spar_geometric,
    spar_unweighted = aggregate_spar_unweighted,
    median = calculate_spar_median,
    mean = calculate_spar_mean,
    stop("Invalid method: ", method, call. = FALSE)
  )
}

#' @rdname calculate_spar_index-internal
#' @keywords internal
#' @noRd
run_spar_index_method <- function(method, dataset, period_variable,
                                  dependent_variable, appraisal_variable,
                                  grouping_variables, reference_period_pattern) {
  calculate_spar_component(
    dataset, period_variable, dependent_variable, appraisal_variable,
    grouping_variables, reference_period_pattern,
    aggregate_function = get_spar_index_method_function(method),
    include_aggregates = method != "spar_unweighted"
  )
}

#' @rdname calculate_spar_index-internal
#' @keywords internal
#' @noRd
calculate_single_spar_index_method <- function(method, dataset, period_variable,
                                               dependent_variable, appraisal_variable,
                                               grouping_variables, index_type_variable,
                                               reference_period_pattern, chained,
                                               base_value) {
  if (method %in% c("median", "mean")) {
    baseline_function <- get_spar_index_method_function(method)
    return(baseline_function(
      dataset, period_variable, dependent_variable,
      if (!is.null(index_type_variable)) index_type_variable else grouping_variables,
      if (chained) base_value else 100
    ))
  }
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

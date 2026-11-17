#' Calculate a sales price appraisal ratio index
#'
#' Calculates arithmetic, geometric, or unweighted sales price appraisal ratio
#' (SPAR) indices from transaction prices and appraisal values. Each
#' `grouping_variables` combination defines a short-term series, normalized by
#' its reference-period ratio. No hedonic regression is fitted.
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
#' @param parallel Logical; run multiple methods with the REPS parallel helper
#'   and bootstrap replicates on snow workers. Default FALSE.
#' @param index_contribution Logical; calculate leave-one-row/unit-out index and
#'   period-on-period contribution for a single method. Default FALSE.
#' @param margins Logical; add bootstrap margins around the original index.
#'   Default FALSE.
#' @param ... Additional options:
#' \itemize{
#'   \item \code{unit_variable}: Index contribution unit identifier, default NULL.
#'   \item \code{index_contribution_period}: Contribution period, default latest.
#'   \item \code{bootstrap_replicates}: Bootstrap repetitions, default 500.
#'   \item \code{confidence_level}: Confidence level, default 0.95.
#'   \item \code{bootstrap_strata}: Optional strata columns, default NULL.
#' }
#' @return One data frame, or a named list of data frames for multiple methods.
#'   Each contains character `period`, the grouping identifiers, ratios
#'   `ratio_numerator` and `ratio_denominator`, `growth_rate`, and `Index`.
#'   Arithmetic/geometric output also contains `numerator_numerator` and
#'   `numerator_denominator`. Chaining adds `chaining_true_false`,
#'   `growth_rate_long`, `yoy_change`, `chain_long`, and `growth_factor`
#'   (the actual period-on-period factor of `Index`). If all groups lack a
#'   reference, the result has zero rows.
#'   With `index_contribution = TRUE`, returns a list containing `Index` and
#'   `Index_contribution`, including grouping identifiers. An exclusion that
#'   leaves the target index uncomputable has missing exclusion/difference values.
#'   Margins add `bootstrap_variance`, `lower_margin`, and `upper_margin` to
#'   index frames. A period/group needs at least 2 and at least 80% of the
#'   requested replicates; otherwise its margins are NA with a warning.
#'   Failed replicates warn and are skipped. Use `set.seed()` for reproducibility.
#'   Filter grouped output to one series before using [plot_price_index()].
#' @author Kai Cheung, Vivek Gajadhar
#' @examples
#' data("arithmetic_data")
#' spar <- calculate_spar_index(
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
calculate_spar_index <- function(dataset,
                           method,
                           period_variable,
                           dependent_variable,
                           appraisal_variable,
                           grouping_variables,
                           index_type_variable = NULL,
                           reference_period_pattern = "01$",
                           chained = FALSE,
                           parallel = FALSE,
                           index_contribution = FALSE,
                           margins = FALSE,
                           ...) {
  method <- validate_spar_index_methods(method)
  extra_args <- list(...)
  options <- list(unit_variable = NULL, index_contribution_period = NULL,
                  bootstrap_replicates = 500,
                  confidence_level = 0.95, bootstrap_strata = NULL)
  if (length(extra_args) && (is.null(names(extra_args)) || anyNA(names(extra_args)) ||
      any(!nzchar(names(extra_args))) || anyDuplicated(names(extra_args)))) {
    stop("All SPAR options in `...` must have unique names.", call. = FALSE)
  }
  unknown <- setdiff(names(extra_args), names(options))
  if (length(unknown)) stop("Unknown SPAR argument(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  options[names(extra_args)] <- extra_args
  validate_spar_index_options(
    grouping_variables, index_type_variable, chained, parallel
  )
  validate_spar_input(
    dataset, period_variable, dependent_variable, appraisal_variable,
    grouping_variables
  )
  validate_spar_contribution_options(
    method, dataset, index_contribution, options$unit_variable,
    options$index_contribution_period
  )
  validate_bootstrap_margin_options(
    margins, options$bootstrap_replicates, options$confidence_level,
    options$bootstrap_strata, dataset
  )
  input_dataset <- dataset
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
        reference_period_pattern, chained
      )
    },
    parallel = parallel && length(method) > 1L,
    fallback_message = "Parallel SPAR method calculation failed; falling back to sequential calculation."
  )
  if (margins) {
    for (i in seq_along(method)) {
      method_name <- method[i]
      result[[i]] <- calculate_bootstrap_margins(
        input_dataset, result[[i]], options$bootstrap_replicates,
        options$bootstrap_strata,
        calculate_index_function = function(sample_data) {
          calculate_spar_index(
            dataset = sample_data, method = method_name, period_variable = period_variable,
            dependent_variable = dependent_variable, appraisal_variable = appraisal_variable,
            grouping_variables = grouping_variables, index_type_variable = index_type_variable,
            reference_period_pattern = reference_period_pattern, chained = chained,
            parallel = FALSE, index_contribution = FALSE, margins = FALSE
          )
        },
        method = method_name, index_variables = c("period", grouping_variables),
        confidence_level = options$confidence_level, parallel = parallel
      )
    }
  }
  if (length(method) == 1L) {
    if (index_contribution) {
      recalculate <- function(target_dataset) {
        prepared <- prepare_spar_data(
          target_dataset, period_variable, dependent_variable, appraisal_variable,
          grouping_variables, reference_period_pattern
        )
        calculate_single_spar_index_method(
          method, prepared, period_variable, dependent_variable, appraisal_variable,
          grouping_variables, index_type_variable, reference_period_pattern,
          chained
        )
      }
      contribution <- calculate_spar_index_contribution(
        input_dataset, result[[1L]], period_variable, grouping_variables,
        if (chained) index_type_variable else grouping_variables,
        recalculate, options$unit_variable, options$index_contribution_period,
        parallel
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
  c("arithmetic", "geometric", "unweighted")
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
    arithmetic = aggregate_spar_arithmetic,
    geometric = aggregate_spar_geometric,
    unweighted = aggregate_spar_unweighted,
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
    include_aggregates = method != "unweighted"
  )
}

#' @rdname calculate_spar_index-internal
#' @keywords internal
#' @noRd
calculate_single_spar_index_method <- function(method, dataset, period_variable,
                                               dependent_variable, appraisal_variable,
                                               grouping_variables, index_type_variable,
                                               reference_period_pattern, chained) {
  result <- run_spar_index_method(
    method, dataset, period_variable, dependent_variable,
    appraisal_variable, grouping_variables, reference_period_pattern
  )
  if (chained) {
    result <- chain_spar_index(
      result, period_variable, index_type_variable, reference_period_pattern
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

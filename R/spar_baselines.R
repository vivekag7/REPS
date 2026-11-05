#' Validate optional price baselines without requiring appraisal inputs
#' @keywords internal
#' @noRd
validate_spar_baseline_input <- function(dataset, period_variable, dependent_variable, grouping_variables) {
  if (!is.data.frame(dataset) || !nrow(dataset) || anyDuplicated(names(dataset))) {
    stop("`dataset` must be a non-empty data frame with unique column names.", call. = FALSE)
  }
  validate_spar_column_names(period_variable, "period_variable", scalar = TRUE)
  validate_spar_column_names(dependent_variable, "dependent_variable", scalar = TRUE)
  validate_input(dataset, period_variable, dependent_variable, NULL, NULL,
    require_explanatory_variables = FALSE)
  if (any(!is.finite(dataset[[dependent_variable]][!is.na(dataset[[dependent_variable]])]))) {
    stop("Baseline prices must be finite.", call. = FALSE)
  }
  if (!all(grouping_variables %in% names(dataset))) stop("Dataset is missing baseline grouping columns.", call. = FALSE)
  if (any(grouping_variables %in% c(period_variable, dependent_variable, "period", "Index"))) {
    stop("Baseline grouping columns must be distinct from price, period, and output columns.", call. = FALSE)
  }
  for (column in grouping_variables) {
    values <- dataset[[column]]
    if (!(is.character(values) || is.factor(values) || (is.numeric(values) && !is.complex(values))) ||
        anyNA(values) || any(!nzchar(trimws(as.character(values)))) ||
        (is.numeric(values) && any(!is.finite(values)))) {
      stop("Baseline grouping identifiers must not be missing or blank.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Reuse the existing median index calculation for independent comparison series
#' @keywords internal
#' @noRd
calculate_spar_median <- function(dataset, period_variable, dependent_variable,
                                   grouping_variables, base_value = 100) {
  calculate_spar_price_baseline(dataset, period_variable, dependent_variable,
    grouping_variables, base_value, calculate_median, "Median")
}

#' Calculate an arithmetic-mean price baseline for independent comparison series
#' @keywords internal
#' @noRd
calculate_spar_mean <- function(dataset, period_variable, dependent_variable,
                                 grouping_variables, base_value = 100) {
  calculate_spar_price_baseline(dataset, period_variable, dependent_variable,
    grouping_variables, base_value, calculate_spar_mean_series, "Mean")
}

#' Normalize arithmetic-mean prices with the existing REPS index helpers
#' @keywords internal
#' @noRd
calculate_spar_mean_series <- function(dataset, period_variable, dependent_variable) {
  prices <- split(dataset[[dependent_variable]], as.character(dataset[[period_variable]]))
  mean_prices <- vapply(prices, mean, numeric(1))
  periods <- names(mean_prices)
  format_index_output(periods, calculate_index(periods, mean_prices))
}

#' Apply a price baseline consistently across independent series
#' @keywords internal
#' @noRd
calculate_spar_price_baseline <- function(dataset, period_variable, dependent_variable,
                                           grouping_variables, base_value,
                                           calculation_function, label) {
  keep <- !is.na(dataset[[period_variable]]) &
    nzchar(as.character(dataset[[period_variable]])) & !is.na(dataset[[dependent_variable]])
  dataset <- dataset[keep, , drop = FALSE]
  if (!nrow(dataset)) stop(paste(label, "index calculation requires at least one complete price-period observation."))
  calculate_one <- function(data) {
    result <- calculation_function(data, period_variable, dependent_variable)
    result$Index <- result$Index * base_value / 100
    if (any(!is.finite(result$Index))) stop(paste(label, "index calculation requires finite index levels."))
    result
  }
  if (!length(grouping_variables)) return(calculate_one(dataset))
  grouped <- dplyr::group_by(dataset, dplyr::across(dplyr::all_of(grouping_variables)))
  result <- dplyr::group_modify(grouped, function(.x, .y) calculate_one(.x))
  as.data.frame(dplyr::ungroup(result))
}

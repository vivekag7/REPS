#' Validate SPAR column-name arguments
#' @keywords internal
#' @noRd
validate_spar_column_names <- function(value, argument, scalar = FALSE) {
  if (!is.character(value) || !length(value) || anyNA(value) ||
      any(!nzchar(trimws(value))) || anyDuplicated(value) ||
      (scalar && length(value) != 1L)) {
    stop("`", argument, "` must contain ",
         if (scalar) "one column name." else "unique column names.", call. = FALSE)
  }
}

#' Validate SPAR scalar options and series identifiers
#' @keywords internal
#' @noRd
validate_spar_index_options <- function(grouping_variables, index_type_variable,
                                        chained, base_value, month_modulo, parallel) {
  for (argument in c("chained", "parallel")) {
    value <- get(argument)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", argument, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  validate_spar_column_names(grouping_variables, "grouping_variables")
  if (chained || !is.null(index_type_variable)) {
    validate_spar_column_names(index_type_variable, "index_type_variable")
    if (!all(index_type_variable %in% grouping_variables)) {
      stop("Every `index_type_variable` must occur in `grouping_variables`.", call. = FALSE)
    }
  }
  if (!is.numeric(base_value) || is.complex(base_value) || length(base_value) != 1L ||
      !is.finite(base_value) || base_value <= 0) {
    stop("`base_value` must be one positive finite number.", call. = FALSE)
  }
  if (!is.numeric(month_modulo) || is.complex(month_modulo) || length(month_modulo) != 1L ||
      !is.finite(month_modulo) || month_modulo <= 1 || month_modulo != floor(month_modulo)) {
    stop("`month_modulo` must be one finite integer greater than 1.", call. = FALSE)
  }
}

#' Validate SPAR observations before aggregation
#' @keywords internal
#' @noRd
validate_spar_input <- function(dataset, period_variable, dependent_variable,
                                appraisal_variable, grouping_variables) {
  if (!is.data.frame(dataset) || nrow(dataset) == 0L || anyDuplicated(names(dataset))) {
    stop("`dataset` must be a non-empty data frame with unique column names.", call. = FALSE)
  }
  for (argument in c("period_variable", "dependent_variable", "appraisal_variable")) {
    validate_spar_column_names(get(argument), argument, scalar = TRUE)
  }
  required <- c(period_variable, dependent_variable, appraisal_variable, grouping_variables)
  if (anyDuplicated(required)) {
    stop("Period, price, appraisal, and grouping columns must be distinct.", call. = FALSE)
  }
  missing <- setdiff(required, names(dataset))
  if (length(missing)) {
    stop("Dataset is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  reserved <- c(
    "period", "Index", "ratio_numerator", "ratio_denominator", "growth_rate",
    "numerator_numerator", "numerator_denominator", "growth_factor", "chaining_true_false"
  )
  if (any(grouping_variables %in% reserved)) {
    stop("`grouping_variables` must not use reserved SPAR output column names.", call. = FALSE)
  }
  if (period_variable %in% setdiff(reserved, "period")) {
    stop("`period_variable` must not use another SPAR output column name.", call. = FALSE)
  }
  for (column in c(period_variable, grouping_variables)) {
    values <- dataset[[column]]
    if (!(is.character(values) || is.factor(values) || (is.numeric(values) && !is.complex(values))) ||
        anyNA(values) || any(!nzchar(trimws(as.character(values)))) ||
        (is.numeric(values) && any(!is.finite(values)))) {
      stop("Period and grouping columns must contain non-missing, non-blank, finite identifiers: ",
           column, call. = FALSE)
    }
  }
  for (column in c(dependent_variable, appraisal_variable)) {
    values <- dataset[[column]]
    if (!is.numeric(values) || is.complex(values)) {
      stop("Price and appraisal columns must be numeric: ", column, call. = FALSE)
    }
    observed <- values[!is.na(values)]
    if (any(!is.finite(observed)) || any(observed <= 0)) {
      stop("Observed price and appraisal values must be strictly positive and finite: ",
           column, call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Prepare SPAR groups with unambiguous reference periods
#' @keywords internal
#' @noRd
prepare_spar_data <- function(dataset, period_variable, dependent_variable,
                               appraisal_variable, grouping_variables,
                               reference_period_pattern) {
  if (!is.character(reference_period_pattern) || length(reference_period_pattern) != 1L ||
      is.na(reference_period_pattern) || !nzchar(reference_period_pattern)) {
    stop("`reference_period_pattern` must be one non-empty regular expression.", call. = FALSE)
  }
  dataset <- as.data.frame(dataset[c(period_variable, grouping_variables,
                                    dependent_variable, appraisal_variable)])
  dataset[[period_variable]] <- as.character(dataset[[period_variable]])
  keys <- unique(dataset[c(grouping_variables, period_variable)])
  matches <- tryCatch(
    suppressWarnings(grepl(reference_period_pattern, keys[[period_variable]])),
    error = function(error) stop("`reference_period_pattern` is not a valid regular expression.", call. = FALSE)
  )
  references <- keys[matches, grouping_variables, drop = FALSE]
  if (anyDuplicated(references)) {
    stop("`reference_period_pattern` must match at most one period in each group.", call. = FALSE)
  }
  missing <- dplyr::anti_join(unique(keys[grouping_variables]), references, by = grouping_variables)
  if (nrow(missing)) {
    warning(nrow(missing), " group(s) have no matching reference period and will be omitted.", call. = FALSE)
  }
  dplyr::semi_join(dataset, references, by = grouping_variables)
}

#' Arithmetic SPAR aggregates on complete pairs
#' @keywords internal
#' @noRd
aggregate_spar_arithmetic <- function(prices, appraisals) {
  price <- mean(prices)
  appraisal <- mean(appraisals)
  c(numerator_numerator = price, numerator_denominator = appraisal,
    ratio_numerator = price / appraisal)
}

#' Geometric SPAR aggregates on complete pairs
#' @keywords internal
#' @noRd
aggregate_spar_geometric <- function(prices, appraisals) {
  price <- calculate_geometric_average(prices)
  appraisal <- calculate_geometric_average(appraisals)
  c(numerator_numerator = price, numerator_denominator = appraisal,
    ratio_numerator = price / appraisal)
}

#' Unweighted SPAR aggregate on complete pairs
#' @keywords internal
#' @noRd
aggregate_spar_unweighted <- function(prices, appraisals) {
  c(ratio_numerator = mean(prices / appraisals))
}

#' Calculate one central SPAR component using the selected aggregate
#' @keywords internal
#' @noRd
calculate_spar_component <- function(dataset, period_variable, dependent_variable,
                                      appraisal_variable, grouping_variables,
                                      reference_period_pattern, aggregate_function,
                                      include_aggregates) {
  if (!nrow(dataset)) {
    result <- dataset[c(period_variable, grouping_variables)]
    columns <- c(if (include_aggregates) c("numerator_numerator", "numerator_denominator"),
                 "ratio_numerator", "ratio_denominator", "growth_rate")
    for (column in columns) result[[column]] <- numeric()
    return(result)
  }
  grouped <- dplyr::group_by(dataset, dplyr::across(dplyr::all_of(c(period_variable, grouping_variables))))
  result <- dplyr::group_modify(grouped, function(.x, .y) {
    prices <- .x[[dependent_variable]]
    appraisals <- .x[[appraisal_variable]]
    paired <- !is.na(prices) & !is.na(appraisals)
    if (!any(paired)) {
      stop("At least one period/group has no complete price-appraisal pair.", call. = FALSE)
    }
    as.data.frame(as.list(aggregate_function(prices[paired], appraisals[paired])))
  })
  result <- as.data.frame(dplyr::ungroup(result))
  if (any(!is.finite(result$ratio_numerator) | result$ratio_numerator <= 0)) {
    stop("SPAR aggregation requires positive finite ratios.", call. = FALSE)
  }
  reference <- result[grepl(reference_period_pattern, result[[period_variable]]),
                      c(grouping_variables, "ratio_numerator"), drop = FALSE]
  names(reference)[names(reference) == "ratio_numerator"] <- "ratio_denominator"
  result <- dplyr::inner_join(result, reference, by = grouping_variables)
  result$growth_rate <- result$ratio_numerator / result$ratio_denominator
  if (any(!is.finite(result$growth_rate) | result$growth_rate <= 0)) {
    stop("SPAR normalization requires positive finite growth rates.", call. = FALSE)
  }
  result
}

#' Validate numeric segment boundaries and unique independent chain series
#' @keywords internal
#' @noRd
validate_spar_chain_data <- function(dataset, period_variable, grouping_variables,
                                      index_type_variable, month_modulo,
                                      reference_period_pattern) {
  if (!nrow(dataset)) return(invisible(TRUE))
  periods <- suppressWarnings(as.numeric(dataset[[period_variable]]))
  if (any(!is.finite(periods) | periods <= 0 | periods != floor(periods))) {
    stop("Chained SPAR requires positive integer numeric or numeric-like periods.", call. = FALSE)
  }
  keys <- unique(dataset[c(grouping_variables, period_variable)])
  reference_matches <- grepl(reference_period_pattern, keys[[period_variable]])
  keys[[period_variable]] <- as.numeric(keys[[period_variable]])
  if (any(keys[[period_variable]][reference_matches] %% month_modulo != 1)) {
    stop("The chaining reference period must match a segment start selected by `month_modulo`.", call. = FALSE)
  }
  if (anyDuplicated(keys[c(index_type_variable, period_variable)])) {
    stop("Chained SPAR requires one observation per period and index-type series; refine `index_type_variable`.", call. = FALSE)
  }
  groups <- dplyr::group_split(dplyr::group_by(keys, dplyr::across(dplyr::all_of(grouping_variables))))
  valid <- vapply(groups, function(group) {
    values <- group[[period_variable]]
    length(unique(floor(values / month_modulo))) == 1L && min(values) %% month_modulo == 1
  }, logical(1))
  if (any(!valid)) {
    stop("Each chained grouping series must occupy one numeric segment and begin where period %% month_modulo == 1.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Chain SPAR short-term series using the segment-start convention
#' @keywords internal
#' @noRd
chain_spar_index <- function(result, period_variable, index_type_variable,
                              base_value, month_modulo) {
  if (!nrow(result)) {
    result$chaining_true_false <- logical()
    result$growth_factor <- numeric()
    result$Index <- numeric()
    return(result)
  }
  grouped <- dplyr::group_by(result, dplyr::across(dplyr::all_of(index_type_variable)))
  result <- dplyr::group_modify(grouped, function(.x, .y) {
    periods <- as.numeric(.x[[period_variable]])
    .x <- .x[order(periods), , drop = FALSE]
    periods <- sort(periods)
    .x$chaining_true_false <- periods %% month_modulo == 1 & seq_along(periods) > 1L
    growth <- .x$growth_rate
    .x$growth_factor <- ifelse(
      .x$chaining_true_false, growth, growth / c(NA_real_, utils::head(growth, -1L))
    )
    .x$growth_factor[1L] <- 1
    .x$Index <- base_value * cumprod(.x$growth_factor)
    .x
  })
  result <- as.data.frame(dplyr::ungroup(result))
  if (any(!is.finite(result$Index) | result$Index <= 0)) {
    stop("SPAR chaining requires positive finite index levels.", call. = FALSE)
  }
  result
}

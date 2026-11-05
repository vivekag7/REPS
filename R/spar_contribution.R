#' Validate optional contribution inputs for the SPAR index hub
#' @keywords internal
#' @noRd
validate_spar_contribution_options <- function(method, dataset, index_contribution,
                                                unit_variable, index_contribution_period) {
  if (!is.logical(index_contribution) || length(index_contribution) != 1L || is.na(index_contribution)) {
    stop("`index_contribution` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!index_contribution) return(invisible(TRUE))
  if (length(method) != 1L) {
    stop("`index_contribution = TRUE` requires a single method.", call. = FALSE)
  }
  if (!is.null(unit_variable)) {
    validate_spar_column_names(unit_variable, "unit_variable", scalar = TRUE)
    if (!unit_variable %in% names(dataset)) stop("Dataset is missing `unit_variable`.", call. = FALSE)
    if (unit_variable %in% c("period", "Index_excl_observation", "Index_original", "Index_difference",
                             "PoP_excl_observation", "PoP_original", "PoP_difference")) {
      stop("`unit_variable` conflicts with a contribution output column.", call. = FALSE)
    }
  }
  if (!is.null(index_contribution_period) &&
      (!(is.character(index_contribution_period) || is.numeric(index_contribution_period)) ||
       length(index_contribution_period) != 1L || anyNA(index_contribution_period) ||
       !nzchar(as.character(index_contribution_period)))) {
    stop("`index_contribution_period` must identify one period.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Adapt grouped SPAR results to the existing REPS contribution calculation
#' @keywords internal
#' @noRd
calculate_spar_index_contribution <- function(dataset, index_output, period_variable,
                                               grouping_variables, series_variables,
                                               calculate_index_function, unit_variable,
                                               index_contribution_period, parallel) {
  if (!nrow(index_output)) stop("Index contribution requires a non-empty index.", call. = FALSE)
  if (is.null(index_contribution_period)) {
    index_contribution_period <- utils::tail(sort(unique(index_output$period)), 1L)
  }
  target_period <- as.character(index_contribution_period)
  target_index <- index_output[index_output$period == target_period, , drop = FALSE]
  if (!nrow(target_index)) {
    stop("`index_contribution_period` is not present in the index.", call. = FALSE)
  }

  # Preserve original row identity before joins and series selection.
  unit_column <- unit_variable
  if (is.null(unit_column)) {
    unit_column <- utils::tail(make.unique(c(names(dataset), ".spar_contribution_row_id")), 1L)
    dataset[[unit_column]] <- as.character(seq_len(nrow(dataset)))
  }
  valid_groups <- unique(index_output[grouping_variables])
  if (length(grouping_variables)) dataset <- dplyr::semi_join(dataset, valid_groups, by = grouping_variables)
  series <- unique(target_index[series_variables])
  if (!length(series_variables)) series <- data.frame(.single_series = 1L)

  results <- lapply(seq_len(nrow(series)), function(i) {
    if (length(series_variables)) {
      series_key <- series[i, , drop = FALSE]
      series_data <- dplyr::semi_join(dataset, series_key, by = series_variables)
      series_index <- dplyr::semi_join(index_output, series_key, by = series_variables)
    } else {
      series_data <- dataset
      series_index <- index_output
    }
    unit_ids <- series_data[[unit_column]][as.character(series_data[[period_variable]]) == target_period]
    if (anyNA(unit_ids) || any(!nzchar(trimws(as.character(unit_ids))))) {
      stop("Contribution unit identifiers must not be missing or blank in the analyzed period.", call. = FALSE)
    }
    recalculate <- function(target_dataset) {
      unavailable <- function() data.frame(period = target_period, Index = NA_real_)
      if (!any(as.character(target_dataset[[period_variable]]) == target_period)) {
        return(unavailable())
      }
      output <- tryCatch(calculate_index_function(target_dataset), error = function(error) {
        if (grepl("Each period/group must have enough observed values", conditionMessage(error), fixed = TRUE) ||
            grepl("^(Mean|Median) index calculation requires", conditionMessage(error))) {
          return(unavailable())
        }
        stop(error)
      })
      if (!nrow(output)) return(unavailable())
      output
    }
    contribution <- calculate_index_contribution(
      dataset = series_data, index_output = series_index,
      period_variable = period_variable, calculate_index_function = recalculate,
      unit_variable = unit_column, index_contribution_period = target_period,
      parallel = parallel
    )
    unavailable <- !is.finite(contribution$Index_excl_observation)
    contribution[unavailable, c("Index_excl_observation", "Index_difference",
                               "PoP_excl_observation", "PoP_difference")] <- NA_real_
    if (is.null(unit_variable)) names(contribution)[names(contribution) == unit_column] <- "row_id"
    target <- series_index[series_index$period == target_period, , drop = FALSE]
    for (column in grouping_variables) contribution[[column]] <- rep(target[[column]][1L], nrow(contribution))
    contribution[unique(c(grouping_variables, names(contribution)))]
  })
  result <- as.data.frame(dplyr::bind_rows(results))
  row.names(result) <- NULL
  result
}

#' Calculate Contribution to Index Mutation
#'
#' Calculates how much each observation, or each unit group, contributes to the
#' index mutation in one selected period by recalculating the index while
#' excluding that observation or unit.
#'
#' @author Farley Ishaak, Egbert Hardeman, Vivek Gajadhar
#' @param dataset Data frame with input data.
#' @param index_output Data frame with the original index output.
#' @param period_variable Name of the period column.
#' @param calculate_index_function Function that recalculates the index for a target dataset.
#' @param unit_variable Optional column name identifying units to exclude as groups. If `NULL`, each row in the target period is treated as one unit.
#' @param index_mutation_period Optional period to analyze. If `NULL`, the latest available period is used.
#' @param digits_index Number of digits used for index and period-growth values.
#' @param digits_difference Number of digits used for difference columns.
#' @return Data frame with contribution-to-index-mutation results.
#' @keywords internal
#' @noRd
calculate_contribution_indexmutation <- function(dataset,
                                                 index_output,
                                                 period_variable,
                                                 calculate_index_function,
                                                 unit_variable = NULL,
                                                 index_mutation_period = NULL,
                                                 digits_index = 4,
                                                 digits_difference = 2) {
  validate_indexmutation_inputs(
    dataset = dataset,
    index_output = index_output,
    period_variable = period_variable,
    unit_variable = unit_variable
  )

  period_values <- as.character(dataset[[period_variable]])
  if (is.null(index_mutation_period)) {
    index_mutation_period <- utils::tail(sort(unique(period_values)), 1)
  }
  index_mutation_period <- as.character(index_mutation_period)

  original_index <- add_period_growth_to_index(index_output)
  original_period_values <- as.character(original_index$period)
  original_row <- original_index[original_period_values == index_mutation_period, , drop = FALSE]

  if (nrow(original_row) != 1) {
    stop("'index_mutation_period' must match exactly one period in the index output.")
  }

  index_original <- round(original_row$Index[1], digits_index)
  period_growth_original <- round(original_row$period_growth[1], digits_index)

  target_data <- dataset[period_values == index_mutation_period, , drop = FALSE]
  other_data <- dataset[period_values != index_mutation_period, , drop = FALSE]

  if (nrow(target_data) == 0) {
    stop("'index_mutation_period' does not match any rows in the dataset.")
  }

  unit_ids <- get_indexmutation_unit_ids(target_data, unit_variable)
  target_data$.indexmutation_unit_id <- unit_ids
  units <- unique(unit_ids)

  contribution <- data.frame(
    .indexmutation_unit_id = units,
    Index_excl_observation = NA_real_,
    Index_original = index_original,
    Index_difference = NA_real_,
    PoP_excl_observation = NA_real_,
    PoP_original = period_growth_original,
    PoP_difference = NA_real_,
    stringsAsFactors = FALSE
  )

  original_names <- names(dataset)

  for (i in seq_along(units)) {
    unit_id <- units[i]
    target_without_unit <- target_data[target_data$.indexmutation_unit_id != unit_id, original_names, drop = FALSE]
    dataset_without_unit <- dplyr::bind_rows(other_data, target_without_unit)

    index_without_unit <- calculate_index_function(dataset_without_unit)
    index_without_unit <- add_period_growth_to_index(index_without_unit)
    excluded_row <- index_without_unit[as.character(index_without_unit$period) == index_mutation_period, , drop = FALSE]

    if (nrow(excluded_row) == 1) {
      index_excl <- round(excluded_row$Index[1], digits_index)
      period_growth_excl <- round(excluded_row$period_growth[1], digits_index)
    } else {
      index_excl <- NA_real_
      period_growth_excl <- NA_real_
    }

    contribution$Index_excl_observation[i] <- index_excl
    contribution$Index_difference[i] <- round(index_original - index_excl, digits_difference)
    contribution$PoP_excl_observation[i] <- period_growth_excl
    contribution$PoP_difference[i] <- round(period_growth_excl - period_growth_original, digits_difference)
  }

  format_indexmutation_output(
    target_data = target_data,
    contribution = contribution,
    unit_variable = unit_variable,
    index_mutation_period = index_mutation_period,
    original_names = original_names
  )
}

#' Validate Index Mutation Inputs
#'
#' Checks whether the dataset and index output contain the columns required by
#' the contribution-to-index-mutation calculation.
#'
#' @author Vivek Gajadhar
#' @param dataset Data frame with input data.
#' @param index_output Data frame with original index output.
#' @param period_variable Name of the period column.
#' @param unit_variable Optional column name identifying unit groups.
#' @return Invisibly returns `TRUE` when valid.
#' @keywords internal
#' @noRd
validate_indexmutation_inputs <- function(dataset, index_output, period_variable, unit_variable = NULL) {
  if (!period_variable %in% names(dataset)) {
    stop("Dataset is missing the period variable required for index mutation.")
  }

  if (!is.data.frame(index_output) ||
      !("period" %in% names(index_output)) ||
      !("Index" %in% names(index_output))) {
    stop("Index mutation requires a regular index data.frame with columns 'period' and 'Index'.")
  }

  if (!is.null(unit_variable) && !unit_variable %in% names(dataset)) {
    stop("Dataset is missing the provided 'unit_variable'.")
  }

  invisible(TRUE)
}

#' Add Period Growth to Index Output
#'
#' Adds the period-over-period growth series used by the index-mutation
#' contribution calculation.
#'
#' @author Vivek Gajadhar
#' @param index_output Data frame with `period` and `Index` columns.
#' @return Index output with an added `period_growth` column.
#' @keywords internal
#' @noRd
add_period_growth_to_index <- function(index_output) {
  validate_indexmutation_inputs(
    dataset = data.frame(period = index_output$period),
    index_output = index_output,
    period_variable = "period"
  )

  index_output <- index_output[order(index_output$period), , drop = FALSE]
  index_output$period_growth <- calculate_growth_rate(index_output$Index) * 100
  index_output
}

#' Get Index Mutation Unit Identifiers
#'
#' Returns unit identifiers for the target period. When no unit variable is
#' supplied, row names are used so every target-period row is excluded once.
#'
#' @author Vivek Gajadhar
#' @param target_data Data frame for the analyzed period.
#' @param unit_variable Optional column name identifying unit groups.
#' @return Character vector of unit identifiers aligned with `target_data`.
#' @keywords internal
#' @noRd
get_indexmutation_unit_ids <- function(target_data, unit_variable = NULL) {
  if (is.null(unit_variable)) {
    return(as.character(row.names(target_data)))
  }

  as.character(target_data[[unit_variable]])
}

#' Format Index Mutation Output
#'
#' Formats row-level or grouped contribution results to match the behavior of
#' the original contribution calculation.
#'
#' @author Vivek Gajadhar
#' @param target_data Data frame for the analyzed period with internal unit IDs.
#' @param contribution Data frame with contribution metrics by unit ID.
#' @param unit_variable Optional column name identifying unit groups.
#' @param index_mutation_period Period analyzed for contribution.
#' @param original_names Original dataset column names.
#' @return Data frame sorted by index difference.
#' @keywords internal
#' @noRd
format_indexmutation_output <- function(target_data,
                                        contribution,
                                        unit_variable,
                                        index_mutation_period,
                                        original_names) {
  if (is.null(unit_variable)) {
    output <- merge(target_data, contribution, by = ".indexmutation_unit_id")
    output <- output[order(output$Index_difference), ]
    output$.indexmutation_unit_id <- NULL
    row.names(output) <- NULL
    return(output[, c(original_names, setdiff(names(output), original_names)), drop = FALSE])
  }

  output <- contribution[order(contribution$Index_difference), ]
  output$period <- index_mutation_period
  names(output)[names(output) == ".indexmutation_unit_id"] <- unit_variable
  row.names(output) <- NULL
  output[, c(unit_variable, "period", setdiff(names(output), c(unit_variable, "period"))), drop = FALSE]
}

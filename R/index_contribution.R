#' Calculate Index Contribution
#'
#' Calculates how much each observation, or each unit group, contributes to the
#' index change in one selected period by recalculating the index while
#' excluding that observation or unit.
#'
#' @author Vivek Gajadhar
#' @param dataset Data frame with input data.
#' @param index_output Data frame with the original index output.
#' @param period_variable Name of the period column.
#' @param calculate_index_function Function that recalculates the index for a target dataset.
#' @param unit_variable Optional column name identifying units to exclude as groups. If `NULL`, each row in the target period is treated as one unit.
#' @param index_contribution_period Optional period to analyze. If `NULL`, the latest available period is used.
#' @param parallel Logical; whether to parallelize leave-one-unit-out recalculations.
#' @return Data frame with index-contribution results.
#' @keywords internal
#' @noRd
calculate_index_contribution <- function(dataset,
                                                 index_output,
                                                 period_variable,
                                                 calculate_index_function,
                                                 unit_variable = NULL,
                                                 index_contribution_period = NULL,
                                                 parallel = FALSE) {
  validate_index_contribution_inputs(
    dataset = dataset,
    index_output = index_output,
    period_variable = period_variable,
    unit_variable = unit_variable
  )

  period_values <- as.character(dataset[[period_variable]])
  if (is.null(index_contribution_period)) {
    index_contribution_period <- utils::tail(sort(unique(period_values)), 1)
  }
  index_contribution_period <- as.character(index_contribution_period)

  original_index <- add_period_growth_to_index(index_output)
  original_period_values <- as.character(original_index$period)
  original_row <- original_index[original_period_values == index_contribution_period, , drop = FALSE]

  if (nrow(original_row) != 1) {
    stop("'index_contribution_period' must match exactly one period in the index output.")
  }

  index_original <- original_row$Index[1]
  period_growth_original <- original_row$period_growth[1]

  target_data <- dataset[period_values == index_contribution_period, , drop = FALSE]
  other_data <- dataset[period_values != index_contribution_period, , drop = FALSE]

  if (nrow(target_data) == 0) {
    stop("'index_contribution_period' does not match any rows in the dataset.")
  }

  unit_ids <- get_index_contribution_unit_ids(target_data, unit_variable)
  target_data$.index_contribution_unit_id <- unit_ids
  units <- unique(unit_ids)

  original_names <- names(dataset)

  contribution <- run_index_contribution_unit_calculations(
    units = units,
    target_data = target_data,
    other_data = other_data,
    original_names = original_names,
    index_contribution_period = index_contribution_period,
    calculate_index_function = calculate_index_function,
    index_original = index_original,
    period_growth_original = period_growth_original,
    parallel = parallel
  )

  format_index_contribution_output(
    target_data = target_data,
    contribution = contribution,
    unit_variable = unit_variable,
    index_contribution_period = index_contribution_period,
    original_names = original_names
  )
}

#' Validate Index Contribution Inputs
#'
#' Checks whether the dataset and index output contain the columns required by
#' the index-contribution calculation.
#'
#' @author Vivek Gajadhar
#' @param dataset Data frame with input data.
#' @param index_output Data frame with original index output.
#' @param period_variable Name of the period column.
#' @param unit_variable Optional column name identifying unit groups.
#' @return Invisibly returns `TRUE` when valid.
#' @keywords internal
#' @noRd
validate_index_contribution_inputs <- function(dataset, index_output, period_variable, unit_variable = NULL) {
  if (!period_variable %in% names(dataset)) {
    stop("Dataset is missing the period variable required for index contribution.")
  }

  if (!is.data.frame(index_output) ||
      !("period" %in% names(index_output)) ||
      !("Index" %in% names(index_output))) {
    stop("Index contribution requires a regular index data.frame with columns 'period' and 'Index'.")
  }

  if (!is.null(unit_variable) && !unit_variable %in% names(dataset)) {
    stop("Dataset is missing the provided 'unit_variable'.")
  }

  invisible(TRUE)
}

#' Add Period Growth to Index Output
#'
#' Adds the period-over-period growth series used by the index-contribution
#' contribution calculation.
#'
#' @author Vivek Gajadhar
#' @param index_output Data frame with `period` and `Index` columns.
#' @return Index output with an added `period_growth` column.
#' @keywords internal
#' @noRd
add_period_growth_to_index <- function(index_output) {
  validate_index_contribution_inputs(
    dataset = data.frame(period = index_output$period),
    index_output = index_output,
    period_variable = "period"
  )

  index_output <- index_output[order(index_output$period), , drop = FALSE]
  index_output$period_growth <- (calculate_growth_rate(index_output$Index) - 1) * 100
  index_output
}

#' Get Index Contribution Unit Identifiers
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
get_index_contribution_unit_ids <- function(target_data, unit_variable = NULL) {
  if (is.null(unit_variable)) {
    return(as.character(row.names(target_data)))
  }

  as.character(target_data[[unit_variable]])
}

#' Format Index Contribution Output
#'
#' Formats contribution results in a consistent compact format:
#' analyzed period, excluded unit identifier, and contribution variables.
#'
#' @author Vivek Gajadhar
#' @param target_data Data frame for the analyzed period with internal unit IDs.
#' @param contribution Data frame with contribution metrics by unit ID.
#' @param unit_variable Optional column name identifying unit groups.
#' @param index_contribution_period Period analyzed for contribution.
#' @param original_names Original dataset column names. Kept for compatibility.
#' @return Data frame with period, unit identifier, and contribution results.
#' @keywords internal
#' @noRd
format_index_contribution_output <- function(target_data,
                                             contribution,
                                             unit_variable,
                                             index_contribution_period,
                                             original_names) {
  output <- contribution[order(contribution$Index_difference), , drop = FALSE]
  
  output$period <- index_contribution_period
  
  if (is.null(unit_variable)) {
    names(output)[names(output) == ".index_contribution_unit_id"] <- "row_id"
    unit_output_variable <- "row_id"
  } else {
    names(output)[names(output) == ".index_contribution_unit_id"] <- unit_variable
    unit_output_variable <- unit_variable
  }
  
  row.names(output) <- NULL
  
  output[, c(
    unit_output_variable,
    "period",
    "Index_excl_observation",
    "Index_original",
    "Index_difference",
    "PoP_excl_observation",
    "PoP_original",
    "PoP_difference"
  ), drop = FALSE]
}

#' Run Index Mutation Unit Calculations
#'
#' Calculates contribution rows sequentially or in parallel.
#'
#' @author Farley Ishaak, Vivek Gajadhar
#' @param units Unit identifiers to exclude one at a time.
#' @param target_data Data for the analyzed period.
#' @param other_data Data for periods outside the analyzed period.
#' @param original_names Original dataset column names.
#' @param index_mutation_period Period analyzed for contribution.
#' @param calculate_index_function Function that recalculates the index for a target dataset.
#' @param index_original Original index value in the analyzed period.
#' @param period_growth_original Original period-over-period growth in the analyzed period.
#' @param parallel Logical; whether parallel execution is requested.
#' @return Data frame with one contribution row per unit.
#' @keywords internal
#' @noRd
run_index_contribution_unit_calculations <- function(units,
                                                target_data,
                                                other_data,
                                                original_names,
                                                index_contribution_period,
                                                calculate_index_function,
                                                index_original,
                                                period_growth_original,
                                                parallel = FALSE) {
  task_function <- function(unit_id) {
    calculate_index_contribution_unit(
      unit_id = unit_id,
      target_data = target_data,
      other_data = other_data,
      original_names = original_names,
      index_contribution_period = index_contribution_period,
      calculate_index_function = calculate_index_function,
      index_original = index_original,
      period_growth_original = period_growth_original
    )
  }
  
  total_runs <- length(units)
  progress_enabled <- total_runs > 0L
  
  if (total_runs == 0L) {
    return(data.frame())
  }
  
  chunk_size <- get_index_contribution_chunk_size(total_runs)
  chunks <- unname(split(units, ceiling(seq_along(units) / chunk_size)))
  
  results <- vector("list", length(chunks))
  completed <- 0L
  
  update_index_contribution_progress(
    current_run = completed,
    total_runs = total_runs,
    progress_enabled = progress_enabled
  )
  
  for (i in seq_along(chunks)) {
    if (isTRUE(parallel)) {
      results[[i]] <- run_parallel_tasks(
        tasks = chunks[[i]],
        task_function = task_function,
        parallel = parallel,
        fallback_message = "Parallel index contribution calculation failed; falling back to sequential calculation."
      )
    } else {
      results[[i]] <- lapply(chunks[[i]], task_function)
    }
    
    completed <- completed + length(chunks[[i]])
    
    update_index_contribution_progress(
      current_run = completed,
      total_runs = total_runs,
      progress_enabled = progress_enabled
    )
  }
  
  do.call(rbind, unlist(results, recursive = FALSE))
}

#' Get Index Contribution Chunk Size
#'
#' Determines the number of units per chunk for progress updates.
#' @author Vivek Gajadhar
#' @param total_runs Total number of recalculations.
#' @param max_updates Maximum number of progress updates.
#' @return Integer chunk size.
#' @keywords internal
#' @noRd
get_index_contribution_chunk_size <- function(total_runs, max_updates = 100L) {
  max(1L, ceiling(total_runs / max_updates))
}

#' Update Index Mutation Progress
#'
#' Writes a single-line progress bar with completed and total recalculation
#' counts.
#' @author Vivek Gajadhar
#' @param current_run Completed recalculations.
#' @param total_runs Total recalculations.
#' @param progress_enabled Logical; whether output should be written.
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
update_index_contribution_progress <- function(current_run, total_runs, progress_enabled) {
  if (!isTRUE(progress_enabled) || total_runs == 0L) {
    return(invisible(NULL))
  }
  
  current_run <- min(current_run, total_runs)
  
  bar_width <- 30L
  completed_width <- floor(bar_width * current_run / total_runs)
  remaining_width <- bar_width - completed_width
  
  bar <- paste0(
    "[",
    strrep("=", completed_width),
    strrep(" ", remaining_width),
    "]"
  )
  
  cat(
    sprintf(
      "\rIndex mutation: %s %d/%d runs completed",
      bar,
      current_run,
      total_runs
    ),
    file = stderr()
  )
  
  if (current_run == total_runs) {
    cat("\n", file = stderr())
  }
  
  invisible(NULL)
}

#' Calculate One Index Contribution Contribution Row
#'
#' Recalculates the index with one unit excluded and returns its contribution
#' values.
#'
#' @author Farley Ishaak, Vivek Gajadhar
#' @param unit_id Unit identifier to exclude.
#' @inheritParams run_index_contribution_unit_calculations
#' @return One-row data frame with contribution values for the excluded unit.
#' @keywords internal
#' @noRd
calculate_index_contribution_unit <- function(unit_id,
                                                      target_data,
                                                      other_data,
                                                      original_names,
                                                      index_contribution_period,
                                                      calculate_index_function,
                                                      index_original,
                                                      period_growth_original) {
  target_without_unit <- target_data[
    target_data$.index_contribution_unit_id != unit_id,
    original_names,
    drop = FALSE
  ]
  dataset_without_unit <- rbind(other_data[, original_names, drop = FALSE], target_without_unit)

  index_without_unit <- calculate_index_function(dataset_without_unit)
  index_without_unit <- add_period_growth_to_index(index_without_unit)
  excluded_row <- index_without_unit[
    as.character(index_without_unit$period) == index_contribution_period,
    ,
    drop = FALSE
  ]

  if (nrow(excluded_row) == 1) {
    index_excl <- excluded_row$Index[1]
    period_growth_excl <- excluded_row$period_growth[1]
  } else {
    index_excl <- NA_real_
    period_growth_excl <- NA_real_
  }

  data.frame(
    .index_contribution_unit_id = unit_id,
    Index_excl_observation = index_excl,
    Index_original = index_original,
    Index_difference = index_original - index_excl,
    PoP_excl_observation = period_growth_excl,
    PoP_original = period_growth_original,
    PoP_difference = period_growth_excl - period_growth_original,
    stringsAsFactors = FALSE
  )
}

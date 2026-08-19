#' Validate SPAR input
#'
#' @inheritParams calculate_spar
#' @return Invisibly returns `TRUE`.
#' @author Kai Cheung, Vivek Gajadhar
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
validate_spar_input <- function(dataset,
                                period_variable,
                                dependent_variable,
                                appraisal_variable,
                                grouping_variables,
                                reference_period_pattern) {
  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame.", call. = FALSE)
  }

  validate_spar_column_names(period_variable, "period_variable", scalar = TRUE)
  validate_spar_column_names(
    dependent_variable,
    "dependent_variable",
    scalar = TRUE
  )
  validate_spar_column_names(
    appraisal_variable,
    "appraisal_variable",
    scalar = TRUE
  )
  validate_spar_column_names(grouping_variables, "grouping_variables")

  required_columns <- unique(c(
    period_variable,
    dependent_variable,
    appraisal_variable,
    grouping_variables
  ))
  missing_columns <- setdiff(required_columns, names(dataset))
  if (length(missing_columns) > 0L) {
    stop(
      "Dataset is missing the following required column(s): ",
      paste(missing_columns, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!is.numeric(dataset[[dependent_variable]])) {
    stop("The dependent variable must be numeric.", call. = FALSE)
  }
  if (!is.numeric(dataset[[appraisal_variable]])) {
    stop("The appraisal variable must be numeric.", call. = FALSE)
  }

  price_values <- dataset[[dependent_variable]]
  appraisal_values <- dataset[[appraisal_variable]]
  observed_values <- c(price_values, appraisal_values)
  observed_values <- observed_values[!is.na(observed_values)]

  if (length(observed_values) == 0L) {
    stop(
      "The dependent and appraisal variables must contain observed values.",
      call. = FALSE
    )
  }
  if (any(!is.finite(observed_values))) {
    stop(
      "The dependent and appraisal variables must not contain infinite values.",
      call. = FALSE
    )
  }
  if (any(observed_values <= 0)) {
    stop(
      "The dependent and appraisal variables must contain strictly positive values.",
      call. = FALSE
    )
  }

  validate_spar_reference_periods(
    dataset = dataset,
    period_variable = period_variable,
    grouping_variables = grouping_variables,
    reference_period_pattern = reference_period_pattern
  )

  invisible(TRUE)
}

#' Validate a logical scalar
#'
#' @param value Value to validate.
#' @param argument Argument name for the error message.
#' @return Invisibly returns `TRUE`.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_scalar_logical <- function(value, argument) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", argument, "` must be either TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a positive numeric scalar
#'
#' @param value Value to validate.
#' @param argument Argument name for the error message.
#' @return Invisibly returns `TRUE`.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_positive_scalar <- function(value, argument) {
  if (!is.numeric(value) || length(value) != 1L ||
      !is.finite(value) || value <= 0) {
    stop(
      "`", argument, "` must be one positive finite number.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate SPAR column names
#'
#' @param value Value to validate.
#' @param argument Argument name for the error message.
#' @param scalar Logical; whether exactly one name is required.
#' @return Invisibly returns `TRUE`.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_spar_column_names <- function(value, argument, scalar = FALSE) {
  invalid <- !is.character(value) || length(value) == 0L ||
    anyNA(value) || any(!nzchar(value)) || anyDuplicated(value)
  if (isTRUE(scalar)) {
    invalid <- invalid || length(value) != 1L
  }
  if (invalid) {
    requirement <- if (isTRUE(scalar)) {
      "a single column name"
    } else {
      "one or more unique column names"
    }
    stop("`", argument, "` must contain ", requirement, ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate SPAR reference periods
#'
#' Every grouping series must have no more than one reference period. Groups
#' without a reference period are allowed but omitted from the result.
#'
#' @inheritParams calculate_spar
#' @return Invisibly returns `TRUE`.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_spar_reference_periods <- function(dataset,
                                            period_variable,
                                            grouping_variables,
                                            reference_period_pattern) {
  if (!is.character(reference_period_pattern) ||
      length(reference_period_pattern) != 1L ||
      is.na(reference_period_pattern) || !nzchar(reference_period_pattern)) {
    stop(
      "`reference_period_pattern` must be one non-empty regular expression.",
      call. = FALSE
    )
  }

  period_values <- as.character(dataset[[period_variable]])
  reference_matches <- tryCatch(
    grepl(reference_period_pattern, period_values),
    error = function(error) {
      stop(
        "`reference_period_pattern` is not a valid regular expression.",
        call. = FALSE
      )
    }
  )
  reference_matches[is.na(reference_matches)] <- FALSE

  group_periods <- unique(dataset[c(grouping_variables, period_variable)])
  grouped_matches <- grepl(
    reference_period_pattern,
    as.character(group_periods[[period_variable]])
  )
  grouped_matches[is.na(grouped_matches)] <- FALSE
  reference_groups <- group_periods[grouped_matches, , drop = FALSE]
  all_groups <- unique(group_periods[grouping_variables])

  reference_counts <- dplyr::count(
    reference_groups,
    dplyr::across(dplyr::all_of(grouping_variables)),
    name = ".reference_count"
  )
  if (any(reference_counts$.reference_count > 1L)) {
    stop(
      "`reference_period_pattern` must match at most one period in each group.",
      call. = FALSE
    )
  }

  missing_reference <- dplyr::anti_join(
    all_groups,
    reference_counts,
    by = grouping_variables
  )
  if (nrow(missing_reference) > 0L) {
    warning(
      nrow(missing_reference),
      " group(s) have no matching reference period and will be omitted.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Calculate an arithmetic SPAR index
#'
#' @inheritParams calculate_spar
#' @return A data frame containing a short-term SPAR result.
#' @author Kai Cheung
#' @keywords internal
#' @noRd
calculate_spar_arithmetic <- function(dataset,
                                      period_variable,
                                      dependent_variable,
                                      appraisal_variable,
                                      grouping_variables,
                                      reference_period_pattern) {
  calculate_spar_component(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    appraisal_variable = appraisal_variable,
    grouping_variables = grouping_variables,
    reference_period_pattern = reference_period_pattern,
    method = "arithmetic"
  )
}

#' Calculate a geometric SPAR index
#'
#' @inheritParams calculate_spar
#' @return A data frame containing a short-term SPAR result.
#' @author Kai Cheung
#' @keywords internal
#' @noRd
calculate_spar_geometric <- function(dataset,
                                     period_variable,
                                     dependent_variable,
                                     appraisal_variable,
                                     grouping_variables,
                                     reference_period_pattern) {
  calculate_spar_component(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    appraisal_variable = appraisal_variable,
    grouping_variables = grouping_variables,
    reference_period_pattern = reference_period_pattern,
    method = "geometric"
  )
}

#' Calculate an unweighted SPAR index
#'
#' @inheritParams calculate_spar
#' @return A data frame containing a short-term SPAR result.
#' @author Kai Cheung
#' @keywords internal
#' @noRd
calculate_spar_unweighted <- function(dataset,
                                      period_variable,
                                      dependent_variable,
                                      appraisal_variable,
                                      grouping_variables,
                                      reference_period_pattern) {
  calculate_spar_component(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    appraisal_variable = appraisal_variable,
    grouping_variables = grouping_variables,
    reference_period_pattern = reference_period_pattern,
    method = "unweighted"
  )
}

#' Calculate one short-term SPAR component
#'
#' @inheritParams calculate_spar
#' @param method One canonical SPAR method name.
#' @return A data frame containing aggregated ratios and `growth_rate`.
#' @author Kai Cheung, Vivek Gajadhar
#' @keywords internal
#' @noRd
calculate_spar_component <- function(dataset,
                                     period_variable,
                                     dependent_variable,
                                     appraisal_variable,
                                     grouping_variables,
                                     reference_period_pattern,
                                     method) {
  group_columns <- c(period_variable, grouping_variables)
  grouped_data <- dplyr::group_by(
    dataset,
    dplyr::across(dplyr::all_of(group_columns))
  )

  if (method == "unweighted") {
    numerator <- dplyr::summarise(
      grouped_data,
      ratio_numerator = mean(
        .data[[dependent_variable]] / .data[[appraisal_variable]],
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  } else {
    average_function <- if (method == "arithmetic") {
      function(value) mean(value, na.rm = TRUE)
    } else {
      function(value) exp(mean(log(value), na.rm = TRUE))
    }
    numerator <- dplyr::summarise(
      grouped_data,
      numerator_numerator = average_function(
        paired_spar_values(
          .data[[dependent_variable]],
          .data[[dependent_variable]],
          .data[[appraisal_variable]]
        )
      ),
      numerator_denominator = average_function(
        paired_spar_values(
          .data[[appraisal_variable]],
          .data[[dependent_variable]],
          .data[[appraisal_variable]]
        )
      ),
      .groups = "drop"
    )
    numerator <- dplyr::mutate(
      numerator,
      ratio_numerator = .data$numerator_numerator / .data$numerator_denominator
    )
  }

  if (any(!is.finite(numerator$ratio_numerator))) {
    stop(
      "At least one period/group has no complete price-appraisal pair.",
      call. = FALSE
    )
  }

  period_matches <- grepl(
    reference_period_pattern,
    as.character(numerator[[period_variable]])
  )
  period_matches[is.na(period_matches)] <- FALSE
  denominator <- numerator[period_matches, , drop = FALSE]
  denominator <- dplyr::select(
    denominator,
    dplyr::all_of(grouping_variables),
    ratio_denominator = "ratio_numerator"
  )

  result <- dplyr::inner_join(
    numerator,
    denominator,
    by = grouping_variables
  )
  dplyr::mutate(
    result,
    growth_rate = .data$ratio_numerator / .data$ratio_denominator
  )
}

#' Keep values with a complete price-appraisal pair
#'
#' @param values Values to retain when both pair components are observed.
#' @param prices Transaction price values.
#' @param appraisals Appraisal values.
#' @return A numeric vector with incomplete pairs replaced by `NA_real_`.
#' @author Vivek Gajadhar
#' @keywords internal
#' @noRd
paired_spar_values <- function(values, prices, appraisals) {
  values[is.na(prices) | is.na(appraisals)] <- NA_real_
  values
}

#' Chain short-term SPAR indices
#'
#' @param df A data frame containing short-term SPAR results.
#' @param period_variable Name of the period column.
#' @param index_type_variable Names of columns identifying independent series.
#' @param growth_variable Name of the short-term index column.
#' @param base_value Chained index base value.
#' @param month_modulo Chained segment modulus.
#' @return `df` with `chaining_true_false`, `growth_factor`, and `Index` added.
#' @author Kai Cheung, Vivek Gajadhar
#' @keywords internal
#' @noRd
chain_spar_index <- function(df,
                             period_variable,
                             index_type_variable,
                             growth_variable,
                             base_value,
                             month_modulo) {
  period_number <- suppressWarnings(
    as.numeric(as.character(df[[period_variable]]))
  )
  if (any(!is.finite(period_number))) {
    stop(
      "Chained SPAR calculation requires numeric or numeric-like periods.",
      call. = FALSE
    )
  }
  if (!is.numeric(df[[growth_variable]]) ||
      any(!is.finite(df[[growth_variable]])) ||
      any(df[[growth_variable]] <= 0)) {
    stop(
      "Chained SPAR calculation requires positive finite growth rates.",
      call. = FALSE
    )
  }

  chain_keys <- c(index_type_variable, period_variable)
  if (anyDuplicated(df[chain_keys])) {
    stop(
      paste0(
        "Chained SPAR calculation requires one observation per period and ",
        "index-type series. Add the necessary series identifiers to ",
        "`index_type_variable`."
      ),
      call. = FALSE
    )
  }

  df$.spar_period_number <- period_number
  df <- dplyr::arrange(
    df,
    dplyr::across(dplyr::all_of(index_type_variable)),
    .data$.spar_period_number
  )
  df <- dplyr::group_by(
    df,
    dplyr::across(dplyr::all_of(index_type_variable))
  )
  df <- dplyr::mutate(
    df,
    chaining_true_false =
      .data$.spar_period_number %% month_modulo == 1 & dplyr::row_number() > 1L,
    growth_factor = dplyr::case_when(
      dplyr::row_number() == 1L ~ 1,
      .data$chaining_true_false ~ .data[[growth_variable]],
      TRUE ~ .data[[growth_variable]] / dplyr::lag(.data[[growth_variable]])
    ),
    Index = base_value * cumprod(.data$growth_factor)
  )
  df <- dplyr::ungroup(df)
  df$.spar_period_number <- NULL

  df
}

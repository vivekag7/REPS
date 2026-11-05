# Internal bootstrap support shared by all methods in the hedonic hub.

validate_bootstrap_margin_options <- function(dataset, margins, bootstrap_replicates,
                                              confidence_level, bootstrap_strata) {
  if (!is.logical(margins) || length(margins) != 1L || is.na(margins)) {
    stop("`margins` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!margins) return(invisible(TRUE))
  if (!is.numeric(bootstrap_replicates) || is.complex(bootstrap_replicates) ||
      length(bootstrap_replicates) != 1L || !is.finite(bootstrap_replicates) ||
      bootstrap_replicates < 2 || bootstrap_replicates != floor(bootstrap_replicates) ||
      bootstrap_replicates > .Machine$integer.max) {
    stop("`bootstrap_replicates` must be a finite integer of at least 2.", call. = FALSE)
  }
  if (!is.numeric(confidence_level) || is.complex(confidence_level) ||
      length(confidence_level) != 1L || !is.finite(confidence_level) ||
      confidence_level <= 0 || confidence_level >= 1) {
    stop("`confidence_level` must be a finite number strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.null(bootstrap_strata)) {
    if (!is.character(bootstrap_strata) || !length(bootstrap_strata) ||
        anyNA(bootstrap_strata) || any(!nzchar(trimws(bootstrap_strata))) ||
        anyDuplicated(bootstrap_strata)) {
      stop("`bootstrap_strata` must contain unique column names or be NULL.", call. = FALSE)
    }
    missing <- setdiff(bootstrap_strata, names(dataset))
    if (length(missing)) {
      stop("Dataset is missing `bootstrap_strata` column(s): ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

sample_bootstrap_rows <- function(dataset, bootstrap_strata = NULL) {
  if (is.null(bootstrap_strata)) {
    rows <- sample.int(nrow(dataset), nrow(dataset), replace = TRUE)
  } else {
    strata <- dplyr::group_rows(dplyr::group_by(
      dataset, dplyr::across(dplyr::all_of(bootstrap_strata)), .drop = TRUE
    ))
    rows <- unlist(lapply(strata, function(group_rows) {
      group_rows[sample.int(length(group_rows), length(group_rows), replace = TRUE)]
    }), use.names = FALSE)
  }
  dataset[rows, , drop = FALSE]
}

validate_bootstrap_index_output <- function(index) {
  if (!is.data.frame(index) || !all(c("period", "Index") %in% names(index)) ||
      !nrow(index) || anyNA(index$period) || anyDuplicated(as.character(index$period)) ||
      !is.numeric(index$Index) || is.complex(index$Index)) {
    stop("Bootstrap margins require a non-empty index data.frame with unique `period` labels and numeric `Index`.",
         call. = FALSE)
  }
  invisible(TRUE)
}

calculate_bootstrap_margins <- function(dataset, index_output, bootstrap_replicates,
                                        confidence_level, bootstrap_strata,
                                        calculate_index_function, method) {
  validate_bootstrap_index_output(index_output)
  values <- matrix(NA_real_, nrow = nrow(index_output), ncol = bootstrap_replicates)
  for (replicate in seq_len(bootstrap_replicates)) {
    sample_data <- sample_bootstrap_rows(dataset, bootstrap_strata)
    index <- tryCatch({
      output <- calculate_index_function(sample_data)
      validate_bootstrap_index_output(output)
      output
    }, error = function(error) {
      warning("Bootstrap replicate ", replicate, " for method '", method,
              "' failed: ", conditionMessage(error), call. = FALSE)
      NULL
    })
    if (!is.null(index)) {
      # Replicates can omit or reorder periods; row positions have no meaning.
      matched <- index$Index[match(as.character(index_output$period), as.character(index$period))]
      matched[!is.finite(matched)] <- NA_real_
      values[, replicate] <- matched
    }
  }

  # Require >= 2 values for variance and >= 80% coverage to avoid presenting
  # margins based on a small surviving fraction of the requested bootstrap.
  required <- max(2L, ceiling(0.8 * bootstrap_replicates))
  valid <- rowSums(!is.na(values))
  insufficient <- valid < required
  if (any(insufficient)) {
    warning("Too few valid bootstrap values for method '", method, "': ",
            paste0(index_output$period[insufficient], " (", valid[insufficient], "/",
                   bootstrap_replicates, ")", collapse = ", "),
            ". At least ", required, " required per period; returning NA margins.", call. = FALSE)
  }
  variance <- vapply(seq_len(nrow(values)), function(i) {
    if (insufficient[i]) NA_real_ else stats::var(values[i, ], na.rm = TRUE)
  }, numeric(1))
  width <- stats::qnorm(1 - (1 - confidence_level) / 2) * sqrt(variance)
  index_output$bootstrap_variance <- variance
  index_output$lower_margin <- index_output$Index - width
  index_output$upper_margin <- index_output$Index + width
  index_output
}

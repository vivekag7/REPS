# Internal bootstrap support shared by the hedonic and SPAR hubs.

#' Validate bootstrap margin options
#' @author Kai Cheung, Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_bootstrap_margin_options <- function(margins, bootstrap_replicates,
                                              confidence_level = 0.95,
                                              bootstrap_strata = NULL, dataset = NULL) {
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
      stop("Dataset is missing `bootstrap_strata` column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Build optional strata for boot::boot
#' @author Kai Cheung, Vivek Gajadhar
#' @keywords internal
#' @noRd
make_bootstrap_strata <- function(dataset, bootstrap_strata = NULL) {
  if (is.null(bootstrap_strata)) return(NULL)

  strata <- lapply(dataset[bootstrap_strata], function(value) {
    factor(value, exclude = NULL)
  })
  if (length(strata) == 1L) return(strata[[1L]])

  do.call(interaction, c(strata, list(drop = TRUE, lex.order = TRUE)))
}

#' Validate a bootstrap index frame
#' @author Kai Cheung, Vivek Gajadhar
#' @keywords internal
#' @noRd
validate_bootstrap_index_output <- function(index, index_variables = "period") {
  if (!is.data.frame(index) || !all(c(index_variables, "Index") %in% names(index)) ||
      !nrow(index) || anyNA(index[index_variables]) || anyDuplicated(index[index_variables]) ||
      !is.numeric(index$Index) || is.complex(index$Index)) {
    stop("Bootstrap margins require a non-empty index data.frame with unique index keys and numeric `Index`.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Add generic bootstrap margins to an index
#' @author Kai Cheung, Vivek Gajadhar
#' @keywords internal
#' @noRd
calculate_bootstrap_margins <- function(dataset, index_output, bootstrap_replicates,
                                        bootstrap_strata,
                                        calculate_index_function, method,
                                        index_variables = "period", confidence_level = 0.95,
                                        parallel = FALSE) {
  validate_bootstrap_index_output(index_output, index_variables)

  n_cores <- resolve_parallel_cores(parallel, bootstrap_replicates)
  use_snow <- n_cores > 1L
  n_index_rows <- nrow(index_output)

  original_keys <- as.data.frame(
    lapply(index_output[index_variables], as.character),
    check.names = FALSE
  )
  value_column <- utils::tail(
    make.unique(c(index_variables, ".bootstrap_value")),
    1L
  )

  first_call <- TRUE
  replicate_number <- 0L
  statistic <- function(data, indices) {
    # boot::boot() first evaluates the statistic on the original observations
    # for t0. The already calculated index is authoritative, so avoid an
    # unnecessary extra model fit and only recalculate the R bootstrap samples.
    if (first_call) {
      first_call <<- FALSE
      return(c(index_output$Index, 0))
    }

    replicate_number <<- replicate_number + 1L
    sample_data <- data[indices, , drop = FALSE]

    index <- tryCatch({
      output <- calculate_index_function(sample_data)
      validate_bootstrap_index_output(output, index_variables)
      output
    }, error = function(error) {
      # Warnings from snow workers are not propagated to the parent process.
      if (!use_snow) {
        warning(
          "Bootstrap replicate ", replicate_number, " for method '", method,
          "' failed: ", conditionMessage(error),
          call. = FALSE
        )
      }
      NULL
    })

    if (is.null(index)) {
      return(c(rep(NA_real_, n_index_rows), 1))
    }

    bootstrap_keys <- as.data.frame(
      lapply(index[index_variables], as.character),
      check.names = FALSE
    )
    bootstrap_keys[[value_column]] <- index$Index
    matched <- dplyr::left_join(
      original_keys,
      bootstrap_keys,
      by = index_variables
    )[[value_column]]
    matched[!is.finite(matched)] <- NA_real_
    c(matched, 0)
  }

  boot_args <- list(
    data = dataset,
    statistic = statistic,
    R = as.integer(bootstrap_replicates)
  )
  strata <- make_bootstrap_strata(dataset, bootstrap_strata)
  if (!is.null(strata)) boot_args$strata <- strata

  if (use_snow) {
    cluster <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    # Use the same worker package setup as the existing REPS parallel helper.
    parallel::clusterEvalQ(cluster, {
      if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
        pkgload::load_all(getwd(), export_all = TRUE, quiet = TRUE)
      } else if (requireNamespace("REPS", quietly = TRUE)) {
        library(REPS)
      }
      NULL
    })
    boot_args$parallel <- "snow"
    boot_args$ncpus <- n_cores
    boot_args$cl <- cluster
  } else {
    boot_args$parallel <- "no"
  }

  bootstrap <- do.call(boot::boot, boot_args)
  # The final statistic element is a numeric failed-recalculation flag.
  # This lets us report failures from snow workers on the parent process.
  failed <- sum(bootstrap$t[, n_index_rows + 1L] > 0)
  if (use_snow && failed > 0L) {
    warning(
      failed, " bootstrap replicate(s) for method '", method,
      "' failed on snow workers; their index values were set to NA.",
      call. = FALSE
    )
  }
  values <- t(bootstrap$t[, seq_len(n_index_rows), drop = FALSE])

  # Require >= 2 values for variance and >= 80% coverage to avoid presenting
  # margins based on a small surviving fraction of the requested bootstrap.
  required <- max(2L, ceiling(0.8 * bootstrap_replicates))
  valid <- rowSums(!is.na(values))
  insufficient <- valid < required

  if (any(insufficient)) {
    warning(
      "Too few valid bootstrap values for method '", method, "': ",
      paste0(
        apply(original_keys[insufficient, , drop = FALSE], 1, paste, collapse = "/"),
        " (", valid[insufficient], "/", bootstrap_replicates, ")",
        collapse = ", "
      ),
      ". At least ", required,
      " required per index key; returning NA margins.",
      call. = FALSE
    )
  }

  variance <- vapply(seq_len(nrow(values)), function(i) {
    if (insufficient[i]) NA_real_ else stats::var(values[i, ], na.rm = TRUE)
  }, numeric(1))

  z <- stats::qnorm(1 - (1 - confidence_level) / 2)
  width <- z * sqrt(variance)

  index_output$bootstrap_variance <- variance
  index_output$lower_margin <- index_output$Index - width
  index_output$upper_margin <- index_output$Index + width
  index_output
}

#!/usr/bin/env Rscript

# ==============================================================================
# Known-truth validation of hedonic retransformation methods
# ==============================================================================
#
# PURPOSE
# -------
# The empirical hedonic-data plots show that the methods produce different
# index paths, but movement alone cannot establish correctness. This script
# uses a controlled lognormal data-generating process for which the target
# arithmetic-mean price index is known exactly.
#
# For period t:
#
#   log(P_ti) = mu_t + epsilon_ti,
#   epsilon_ti ~ N(0, sigma_t^2).
#
# Therefore:
#
#   E(P_ti) = exp(mu_t + sigma_t^2 / 2).
#
# An intercept-only regression is sufficient for this validation. It is a
# valid special case of the hedonic model and makes the distinction exact:
#
#   No correction:
#     exp(y_bar_t)
#
#   Leverage-based fitted-value variance correction (paper):
#     exp(y_bar_t + s_t^2 / (2 n))
#
#     because every leverage is h_ii = 1/n in an intercept-only model.
#
#   Residual-variance lognormal mean correction:
#     exp(y_bar_t + s_t^2 / 2).
#
# The paper method uses the sampling variance of the fitted mean, s^2/n,
# where the raw-price expectation requires the observation-level residual
# variance, sigma^2. As n grows, the paper factor converges to 1 even though
# the required lognormal factor exp(sigma^2/2) does not. The simulation checks
# this algebra numerically using bias and RMSE against the known true index.
#
# This validates the estimand under normal, homoskedastic log errors within
# each period. It does not claim that real housing residuals must be normal or
# homoskedastic; those assumptions require separate diagnostics.
#
# RUN
# ---
#
# From an R terminal or VS Code's "Source R File" command:
#
#   setwd("C:/Users/vgaja/Desktop/REPS")
#   source("research/scripts/validate_retransformation_methods_simulation.R")
#
# Or with Rscript:
#
#   Rscript research/scripts/validate_retransformation_methods_simulation.R
#
# Optional first argument: output directory.
#
# OUTPUT
# ------
#   retransformation_method_validation.png
#
# ==============================================================================


# 1. Configuration ------------------------------------------------------------

SEED <- 20260924L
N_SIMULATIONS <- 5000L
N_PER_PERIOD <- 150L
BASE_INDEX <- 100

PERIODS <- paste0("T", 0:7)
MU <- log(c(100, 102, 104, 106, 108, 110, 112, 114))
# The deliberately varying residual standard deviations make the estimand
# difference visible in a single figure. The analytic result in panel D does
# not depend on this particular profile or on the size of the visual effect.
SIGMA <- c(0.10, 0.18, 0.26, 0.34, 0.42, 0.36, 0.28, 0.20)

METHOD_LABELS <- c(
  no_retransformation = "No retransformation correction",
  paper_leverage = "Leverage-based fitted-value variance correction (paper)",
  lognormal_residual = "Residual-variance lognormal mean correction"
)

COLOURS <- c(
  truth = "#2E7D32",
  no_retransformation = "#202020",
  paper_leverage = "#C0392B",
  lognormal_residual = "#1F618D"
)


# 2. Utilities ----------------------------------------------------------------

find_repository_root <- function() {
  file_argument <- grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )
  command_file <- if (length(file_argument) == 1L) {
    sub("^--file=", "", file_argument[[1L]])
  } else {
    character(0)
  }

  source_files <- unlist(lapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) character(0) else as.character(frame$ofile)[[1L]]
  }), use.names = FALSE)
  start_directories <- c(
    if (length(command_file)) dirname(command_file) else character(0),
    if (length(source_files)) dirname(source_files) else character(0),
    getwd()
  )
  start_directories <- unique(start_directories[dir.exists(start_directories)])

  ancestors <- function(path) {
    current <- normalizePath(path, winslash = "/", mustWork = TRUE)
    result <- current
    repeat {
      parent <- dirname(current)
      if (identical(parent, current)) break
      result <- c(result, parent)
      current <- parent
    }
    result
  }
  candidates <- unique(unlist(lapply(start_directories, ancestors)))
  is_repository <- vapply(candidates, function(path) {
    file.exists(file.path(path, "DESCRIPTION")) &&
      dir.exists(file.path(path, "R")) &&
      file.exists(file.path(path, "data", "hedonic_data.rda"))
  }, logical(1))

  if (!any(is_repository)) {
    stop(
      "Could not locate the REPS repository. Open the REPS folder in VS Code ",
      "or run setwd('C:/Users/vgaja/Desktop/REPS') before sourcing this file."
    )
  }
  normalizePath(
    candidates[which(is_repository)[[1L]]],
    winslash = "/",
    mustWork = TRUE
  )
}

safe_range <- function(values, padding_fraction = 0.08, minimum_span = 0.1) {
  value_range <- range(values, finite = TRUE)
  span <- max(diff(value_range), minimum_span)
  value_range + c(-1, 1) * span * padding_fraction
}

normalise_rows <- function(level_matrix) {
  BASE_INDEX * level_matrix / level_matrix[, 1L]
}


# 3. Simulate the three estimators --------------------------------------------

run_simulation <- function() {
  set.seed(SEED)
  period_count <- length(PERIODS)
  sample_log_means <- matrix(
    NA_real_, nrow = N_SIMULATIONS, ncol = period_count
  )
  sample_log_variances <- sample_log_means

  for (period_position in seq_len(period_count)) {
    simulated_log_prices <- matrix(
      stats::rnorm(
        N_SIMULATIONS * N_PER_PERIOD,
        mean = MU[[period_position]],
        sd = SIGMA[[period_position]]
      ),
      nrow = N_SIMULATIONS,
      ncol = N_PER_PERIOD
    )
    period_means <- rowMeans(simulated_log_prices)
    centred <- simulated_log_prices - period_means
    period_variances <- rowSums(centred^2) / (N_PER_PERIOD - 1L)

    sample_log_means[, period_position] <- period_means
    sample_log_variances[, period_position] <- period_variances
  }

  levels_no_correction <- exp(sample_log_means)
  levels_paper <- exp(
    sample_log_means + sample_log_variances / (2 * N_PER_PERIOD)
  )
  levels_lognormal <- exp(
    sample_log_means + sample_log_variances / 2
  )

  estimated_indices <- list(
    no_retransformation = normalise_rows(levels_no_correction),
    paper_leverage = normalise_rows(levels_paper),
    lognormal_residual = normalise_rows(levels_lognormal)
  )

  true_levels <- exp(MU + SIGMA^2 / 2)
  true_index <- BASE_INDEX * true_levels / true_levels[[1L]]

  list(
    estimated_indices = estimated_indices,
    true_index = true_index,
    true_levels = true_levels
  )
}


# 4. Calculate known-truth performance ---------------------------------------

summarise_simulation <- function(simulation) {
  method_summaries <- lapply(names(METHOD_LABELS), function(method) {
    estimates <- simulation$estimated_indices[[method]]
    errors <- sweep(estimates, 2L, simulation$true_index, "-")
    data.frame(
      method = method,
      period = PERIODS,
      truth = simulation$true_index,
      mean_estimate = colMeans(estimates),
      bias = colMeans(errors),
      rmse = sqrt(colMeans(errors^2)),
      stringsAsFactors = FALSE
    )
  })
  period_summary <- do.call(rbind, method_summaries)
  rownames(period_summary) <- NULL

  non_base <- period_summary$period != PERIODS[[1L]]
  overall_summary <- do.call(rbind, lapply(names(METHOD_LABELS), function(method) {
    rows <- period_summary$method == method & non_base
    estimates <- simulation$estimated_indices[[method]][, -1L, drop = FALSE]
    errors <- sweep(estimates, 2L, simulation$true_index[-1L], "-")
    data.frame(
      method = method,
      method_label = unname(METHOD_LABELS[[method]]),
      mean_absolute_period_bias = mean(abs(period_summary$bias[rows])),
      pooled_rmse = sqrt(mean(errors^2)),
      maximum_absolute_period_bias = max(abs(period_summary$bias[rows])),
      stringsAsFactors = FALSE
    )
  }))
  rownames(overall_summary) <- NULL

  list(period = period_summary, overall = overall_summary)
}


# 5. Plot truth, bias, RMSE, and the analytic factor mismatch -----------------

create_validation_plot <- function(summary, simulation, plot_path) {
  x <- seq_along(PERIODS)
  period_summary <- summary$period

  extract_series <- function(column, method) {
    period_summary[period_summary$method == method, column]
  }

  mean_estimates <- lapply(
    names(METHOD_LABELS),
    function(method) extract_series("mean_estimate", method)
  )
  names(mean_estimates) <- names(METHOD_LABELS)
  biases <- lapply(
    names(METHOD_LABELS),
    function(method) extract_series("bias", method)
  )
  names(biases) <- names(METHOD_LABELS)
  rmses <- lapply(
    names(METHOD_LABELS),
    function(method) extract_series("rmse", method)
  )
  names(rmses) <- names(METHOD_LABELS)

  grDevices::png(plot_path, width = 2500, height = 1800, res = 200)
  old_par <- graphics::par(
    mfrow = c(2, 2),
    mar = c(4.5, 5.1, 3.3, 1.3),
    oma = c(0, 0, 3.2, 0),
    las = 1
  )
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)

  all_mean_values <- c(simulation$true_index, unlist(mean_estimates))
  graphics::plot(
    x,
    simulation$true_index,
    type = "l",
    lwd = 4,
    col = COLOURS[["truth"]],
    ylim = safe_range(all_mean_values),
    xaxt = "n",
    xlab = "Period",
    ylab = "Arithmetic-mean price index",
    main = "A. Monte Carlo mean versus known truth"
  )
  for (method in names(METHOD_LABELS)) {
    graphics::lines(
      x,
      mean_estimates[[method]],
      col = COLOURS[[method]],
      lwd = if (method == "no_retransformation") 3.2 else 2.8,
      lty = if (method == "no_retransformation") 2 else 1
    )
  }
  graphics::lines(
    x,
    simulation$true_index,
    lwd = 3.5,
    lty = 3,
    col = COLOURS[["truth"]]
  )
  graphics::points(
    x,
    simulation$true_index,
    pch = 16,
    cex = 0.6,
    col = COLOURS[["truth"]]
  )
  graphics::axis(1, at = x, labels = PERIODS)
  graphics::legend(
    "topleft",
    legend = c("Known true index", unname(METHOD_LABELS)),
    col = unname(COLOURS),
    lty = c(3, 2, 1, 1),
    lwd = c(4, 3.2, 2.8, 2.8),
    pch = c(16, NA, NA, NA),
    bty = "n",
    cex = 0.75
  )

  all_biases <- c(0, unlist(biases))
  graphics::plot(
    x,
    biases[["paper_leverage"]],
    type = "l",
    lwd = 3,
    col = COLOURS[["paper_leverage"]],
    ylim = safe_range(all_biases),
    xaxt = "n",
    xlab = "Period",
    ylab = "Bias (index points)",
    main = "B. Bias relative to known truth"
  )
  graphics::lines(
    x, biases[["lognormal_residual"]],
    col = COLOURS[["lognormal_residual"]], lwd = 3
  )
  graphics::lines(
    x, biases[["no_retransformation"]],
    col = COLOURS[["no_retransformation"]], lwd = 3.2, lty = 2
  )
  graphics::abline(h = 0, col = "#777777", lty = 3)
  graphics::axis(1, at = x, labels = PERIODS)

  all_rmses <- unlist(rmses)
  graphics::plot(
    x,
    rmses[["paper_leverage"]],
    type = "l",
    lwd = 3,
    col = COLOURS[["paper_leverage"]],
    ylim = c(0, max(all_rmses) * 1.08),
    xaxt = "n",
    xlab = "Period",
    ylab = "RMSE (index points)",
    main = "C. Root mean squared error"
  )
  graphics::lines(
    x, rmses[["lognormal_residual"]],
    col = COLOURS[["lognormal_residual"]], lwd = 3
  )
  graphics::lines(
    x, rmses[["no_retransformation"]],
    col = COLOURS[["no_retransformation"]], lwd = 3.2, lty = 2
  )
  graphics::axis(1, at = x, labels = PERIODS)

  sample_sizes <- unique(round(exp(seq(log(5), log(2000), length.out = 250))))
  demonstration_sigma <- 0.40
  paper_factor <- exp(demonstration_sigma^2 / (2 * sample_sizes))
  lognormal_factor <- rep(
    exp(demonstration_sigma^2 / 2),
    length(sample_sizes)
  )
  graphics::plot(
    sample_sizes,
    lognormal_factor,
    type = "l",
    log = "x",
    lwd = 3,
    col = COLOURS[["lognormal_residual"]],
    ylim = c(0.995, max(lognormal_factor) * 1.01),
    xlab = "Observations in period (log scale)",
    ylab = "Multiplicative correction factor",
    main = expression(paste("D. Analytic mismatch when ", sigma == 0.40))
  )
  graphics::lines(
    sample_sizes,
    paper_factor,
    lwd = 3,
    col = COLOURS[["paper_leverage"]]
  )
  graphics::abline(h = 1, col = COLOURS[["no_retransformation"]], lty = 2)
  graphics::legend(
    "right",
    legend = c(
      "exp(sigma^2 / 2)",
      "exp(sigma^2 / (2n))",
      "No correction"
    ),
    col = c(
      COLOURS[["lognormal_residual"]],
      COLOURS[["paper_leverage"]],
      COLOURS[["no_retransformation"]]
    ),
    lty = c(1, 1, 2),
    lwd = 3,
    bty = "n",
    cex = 0.85
  )

  graphics::mtext(
    sprintf(
      "Known-truth retransformation validation (%s simulations; n = %s per period)",
      format(N_SIMULATIONS, big.mark = ","),
      N_PER_PERIOD
    ),
    outer = TRUE,
    font = 2,
    cex = 1.18
  )

  invisible(plot_path)
}


# 6. Main ---------------------------------------------------------------------

main <- function() {
  called_by_rscript <- length(grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )) == 1L
  arguments <- if (called_by_rscript) {
    commandArgs(trailingOnly = TRUE)
  } else {
    character(0)
  }
  repository_root <- find_repository_root()
  output_dir <- if (length(arguments) >= 1L) {
    arguments[[1L]]
  } else {
    file.path(repository_root, "research", "figures")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  plot_path <- file.path(
    output_dir,
    "retransformation_method_validation.png"
  )

  simulation <- run_simulation()
  summary <- summarise_simulation(simulation)
  create_validation_plot(summary, simulation, plot_path)

  cat("Known-truth retransformation validation completed successfully\n\n")
  print(summary$overall, row.names = FALSE, digits = 6)
  cat("\nInterpretation:\n")
  cat(paste0(
    "  Under the stated lognormal DGP, the raw-price mean contains the ",
    "factor exp(sigma^2/2). The leverage-based factor is exp(sigma^2/(2n)) ",
    "in this intercept-only case and converges to 1, so it does not target ",
    "the arithmetic mean.\n"
  ))
  cat("\nPlot: ", normalizePath(plot_path, winslash = "/"), "\n", sep = "")

  invisible(summary)
}

main()

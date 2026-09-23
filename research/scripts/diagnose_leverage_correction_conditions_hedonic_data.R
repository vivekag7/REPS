#!/usr/bin/env Rscript

# ==============================================================================
# Conditions required by the leverage-based fitted-value variance correction
# ==============================================================================
#
# PURPOSE
# -------
# This script diagnoses, on the bundled hedonic_data, the assumptions and
# structural conditions behind the paper's correction
#
#   exp(y_hat_i) * exp(sigma_hat^2 * h_ii / 2).
#
# If the target is the arithmetic mean of an observed lognormal price,
#
#   log(P_i) = x_i' beta + epsilon_i,
#   epsilon_i ~ N(0, sigma^2),
#
# then
#
#   E(P_i | x_i) = exp(x_i' beta) * exp(sigma^2 / 2).
#
# The paper substitutes sigma^2 h_ii, the sampling variance of a fitted log
# mean, for sigma^2, the variance of a new observation around that mean. For
# the two variance terms to be equal observation by observation, either:
#
#   1. h_ii = 1 for every observation, or
#   2. sigma^2 = 0.
#
# In a full-rank OLS model, sum(h_ii) = k. Therefore h_ii = 1 for every row
# requires k = n, leaving zero residual degrees of freedom and making sigma^2
# unestimable. This is a structural incompatibility, not a goodness-of-fit
# preference.
#
# The script checks four things for each period-specific hedonic model:
#
#   A. Are the leverages h_ii equal to the required value 1?
#   B. Does the paper factor equal the raw-price lognormal factor?
#   C. Is the second-stage projection of nonlinear corrected prices onto X
#      exact rather than an approximation?
#   D. Are normality and homoskedasticity diagnostics supportive?
#
# Normality and homoskedasticity tests are sample diagnostics, not proofs.
# The leverage/variance mismatch is algebraic and remains even if both tests
# happen not to reject.
#
# RUN INTERACTIVELY IN VS CODE
# ----------------------------
#
#   setwd("C:/Users/vgaja/Desktop/REPS")
#   source("research/scripts/diagnose_leverage_correction_conditions_hedonic_data.R")
#
# Or with Rscript:
#
#   Rscript research/scripts/diagnose_leverage_correction_conditions_hedonic_data.R
#
# Optional Rscript arguments:
#   1. path to hedonic_data.rda
#   2. output directory
#
# OUTPUT
# ------
#   leverage_correction_condition_diagnostics.png
#
# ==============================================================================


# 1. Configuration ------------------------------------------------------------

NUMERICAL_VARIABLES <- c("floor_area", "dist_trainstation")
CATEGORICAL_VARIABLES <- c("neighbourhood_code", "dummy_large_city")
PERIOD_VARIABLE <- "period"
DEPENDENT_VARIABLE <- "price"
SIGNIFICANCE_LEVEL <- 0.05
PROJECTION_TOLERANCE <- 1e-10

COLOURS <- c(
  required = "#2E7D32",
  paper = "#C0392B",
  secondary = "#1F618D",
  neutral = "#202020"
)


# 2. Locate the repository in Rscript, source(), or VS Code -------------------

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

normalise_existing_path <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

normalise_output_path <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

safe_log_values <- function(values) pmax(values, 1e-6)


# 3. Load and prepare the common analysis sample ------------------------------

load_analysis_data <- function(data_path) {
  data_environment <- new.env(parent = baseenv())
  loaded_objects <- load(data_path, envir = data_environment)
  if (!"hedonic_data" %in% loaded_objects) {
    stop("The data file does not contain an object named 'hedonic_data'")
  }

  raw_data <- data_environment$hedonic_data
  required_columns <- c(
    PERIOD_VARIABLE,
    DEPENDENT_VARIABLE,
    NUMERICAL_VARIABLES,
    CATEGORICAL_VARIABLES
  )
  missing_columns <- setdiff(required_columns, names(raw_data))
  if (length(missing_columns) > 0L) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  analysis_data <- raw_data[, required_columns, drop = FALSE]
  analysis_data <- analysis_data[stats::complete.cases(analysis_data), , drop = FALSE]
  if (nrow(analysis_data) == 0L) stop("No complete observations remain")
  if (any(analysis_data[[DEPENDENT_VARIABLE]] <= 0)) {
    stop("All prices must be strictly positive")
  }

  analysis_data[[PERIOD_VARIABLE]] <- as.character(
    analysis_data[[PERIOD_VARIABLE]]
  )
  for (variable in CATEGORICAL_VARIABLES) {
    analysis_data[[variable]] <- factor(
      analysis_data[[variable]],
      levels = sort(unique(analysis_data[[variable]]))
    )
  }

  list(
    data = analysis_data,
    periods = sort(unique(analysis_data[[PERIOD_VARIABLE]]))
  )
}


# 4. Period-level condition diagnostics --------------------------------------

breusch_pagan_p_value <- function(residuals, X) {
  squared_residuals <- residuals^2
  auxiliary <- stats::lm.fit(x = X, y = squared_residuals)
  total_sum_squares <- sum((squared_residuals - mean(squared_residuals))^2)
  if (total_sum_squares <= 0) return(NA_real_)
  r_squared <- 1 - sum(auxiliary$residuals^2) / total_sum_squares
  statistic <- length(residuals) * max(0, r_squared)
  degrees_of_freedom <- auxiliary$rank - 1L
  stats::pchisq(statistic, df = degrees_of_freedom, lower.tail = FALSE)
}

diagnose_period_models <- function(analysis_data, periods) {
  quality_formula <- stats::reformulate(
    c(NUMERICAL_VARIABLES, CATEGORICAL_VARIABLES),
    intercept = TRUE
  )
  X_all <- stats::model.matrix(quality_formula, data = analysis_data)
  y_all <- log(analysis_data[[DEPENDENT_VARIABLE]])

  period_diagnostics <- vector("list", length(periods))
  observation_diagnostics <- vector("list", length(periods))

  for (i in seq_along(periods)) {
    period <- periods[[i]]
    rows <- analysis_data[[PERIOD_VARIABLE]] == period
    X <- X_all[rows, , drop = FALSE]
    y <- y_all[rows]
    n <- nrow(X)
    k <- ncol(X)

    fit <- stats::lm.fit(x = X, y = y)
    if (fit$rank != k || anyNA(fit$coefficients)) {
      stop("Rank-deficient model in period ", period)
    }
    if (n <= k) stop("No residual degrees of freedom in period ", period)

    fitted_log_price <- drop(X %*% fit$coefficients)
    residuals <- y - fitted_log_price
    sigma2_hat <- sum(residuals^2) / (n - k)
    xtx_inverse <- chol2inv(chol(crossprod(X)))
    leverage <- rowSums((X %*% xtx_inverse) * X)
    if (abs(sum(leverage) - k) > 1e-7) {
      stop("Leverage trace identity failed in period ", period)
    }

    paper_factor <- exp(sigma2_hat * leverage / 2)
    required_factor <- exp(sigma2_hat / 2)
    paper_corrected_prices <- exp(fitted_log_price) * paper_factor
    beta_back <- drop(
      xtx_inverse %*% crossprod(X, paper_corrected_prices)
    )
    projected_prices <- drop(X %*% beta_back)
    projection_errors <- paper_corrected_prices - projected_prices

    shapiro_p <- if (n >= 3L && n <= 5000L) {
      stats::shapiro.test(residuals)$p.value
    } else {
      NA_real_
    }
    bp_p <- breusch_pagan_p_value(residuals, X)

    period_diagnostics[[i]] <- data.frame(
      period = period,
      n = n,
      k = k,
      residual_degrees_of_freedom = n - k,
      full_rank = fit$rank == k,
      sigma2_hat = sigma2_hat,
      mean_leverage = mean(leverage),
      maximum_leverage = max(leverage),
      observations_with_h_equal_one = sum(abs(leverage - 1) < 1e-12),
      required_factor_uplift_percent = 100 * (required_factor - 1),
      mean_paper_factor_uplift_percent = 100 * mean(paper_factor - 1),
      projection_relative_rmse_percent =
        100 * sqrt(mean(projection_errors^2)) / mean(paper_corrected_prices),
      projection_maximum_relative_error_percent =
        100 * max(abs(projection_errors) / paper_corrected_prices),
      projection_exact = max(abs(projection_errors)) < PROJECTION_TOLERANCE,
      shapiro_p_value = shapiro_p,
      breusch_pagan_p_value = bp_p,
      stringsAsFactors = FALSE
    )

    observation_diagnostics[[i]] <- data.frame(
      period = period,
      leverage = leverage,
      variance_fraction_used = leverage,
      paper_factor = paper_factor,
      required_factor = required_factor,
      stringsAsFactors = FALSE
    )
  }

  list(
    period = do.call(rbind, period_diagnostics),
    observation = do.call(rbind, observation_diagnostics),
    design_columns = colnames(X_all)
  )
}


# 5. Plot the failed and supported conditions ---------------------------------

create_condition_plot <- function(diagnostics, plot_path) {
  period <- diagnostics$period
  x <- seq_len(nrow(period))

  grDevices::png(plot_path, width = 2500, height = 1800, res = 200)
  old_par <- graphics::par(
    mfrow = c(2, 2),
    mar = c(4.5, 5.2, 3.4, 1.3),
    oma = c(0, 0, 3.2, 0),
    las = 1
  )
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)

  leverage_range <- range(c(
    period$mean_leverage,
    period$maximum_leverage,
    1
  ))
  graphics::plot(
    x,
    period$maximum_leverage,
    type = "l",
    log = "y",
    lwd = 3,
    col = COLOURS[["paper"]],
    ylim = leverage_range,
    xaxt = "n",
    xlab = "Period",
    ylab = expression("Leverage " * h[ii] * " (log scale)"),
    main = expression(paste("A. Required ", h[ii] == 1, " is not observed"))
  )
  graphics::lines(
    x,
    period$mean_leverage,
    lwd = 3,
    col = COLOURS[["secondary"]]
  )
  graphics::abline(h = 1, lwd = 3, lty = 2, col = COLOURS[["required"]])
  graphics::legend(
    "bottomright",
    legend = c("Maximum observed leverage", "Mean observed leverage", "Required leverage"),
    col = c(COLOURS[["paper"]], COLOURS[["secondary"]], COLOURS[["required"]]),
    lty = c(1, 1, 2),
    lwd = 3,
    bty = "n",
    cex = 0.8
  )

  factor_values <- c(
    period$required_factor_uplift_percent,
    period$mean_paper_factor_uplift_percent
  )
  graphics::plot(
    x,
    period$required_factor_uplift_percent,
    type = "l",
    lwd = 3,
    col = COLOURS[["required"]],
    ylim = range(c(0, factor_values)),
    xaxt = "n",
    xlab = "Period",
    ylab = "Uplift over exp(fitted log price) (%)",
    main = "B. Paper factor uses only a small variance fraction"
  )
  graphics::lines(
    x,
    period$mean_paper_factor_uplift_percent,
    lwd = 3,
    col = COLOURS[["paper"]]
  )
  graphics::legend(
    "topleft",
    legend = c("Required exp(sigma^2/2)", "Paper exp(sigma^2 h_ii/2), mean"),
    col = c(COLOURS[["required"]], COLOURS[["paper"]]),
    lty = 1,
    lwd = 3,
    bty = "n",
    cex = 0.82
  )

  graphics::plot(
    x,
    period$projection_relative_rmse_percent,
    type = "l",
    lwd = 3,
    col = COLOURS[["paper"]],
    ylim = range(c(0, period$projection_relative_rmse_percent)),
    xaxt = "n",
    xlab = "Period",
    ylab = "Projection RMSE (% of mean corrected price)",
    main = "C. Nonlinear corrected prices are not exactly linear in X"
  )
  graphics::abline(h = 0, lty = 2, col = COLOURS[["required"]])

  shapiro_values <- safe_log_values(period$shapiro_p_value)
  bp_values <- safe_log_values(period$breusch_pagan_p_value)
  graphics::plot(
    x,
    shapiro_values,
    type = "l",
    log = "y",
    lwd = 3,
    col = COLOURS[["secondary"]],
    ylim = c(1e-6, 1),
    xaxt = "n",
    xlab = "Period",
    ylab = "Diagnostic p-value (log scale)",
    main = "D. Log-error assumption diagnostics"
  )
  graphics::lines(
    x,
    bp_values,
    lwd = 3,
    col = COLOURS[["paper"]]
  )
  graphics::abline(
    h = SIGNIFICANCE_LEVEL,
    lty = 2,
    lwd = 2,
    col = COLOURS[["neutral"]]
  )
  graphics::legend(
    "bottomleft",
    legend = c("Shapiro-Wilk normality", "Breusch-Pagan homoskedasticity", "5% threshold"),
    col = c(COLOURS[["secondary"]], COLOURS[["paper"]], COLOURS[["neutral"]]),
    lty = c(1, 1, 2),
    lwd = c(3, 3, 2),
    bty = "n",
    cex = 0.8
  )

  axis_positions <- seq.int(1L, nrow(period), by = 4L)
  for (panel in 1:4) {
    graphics::par(mfg = c(if (panel <= 2) 1 else 2, if (panel %% 2 == 1) 1 else 2))
    graphics::axis(
      1,
      at = axis_positions,
      labels = substr(period$period[axis_positions], 1L, 4L),
      las = 2,
      cex.axis = 0.8
    )
  }
  graphics::mtext(
    "Conditions behind the leverage-based fitted-value variance correction",
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
  default_data_path <- file.path(repository_root, "data", "hedonic_data.rda")
  default_output_dir <- file.path(
    repository_root,
    "research",
    "figures"
  )
  data_path <- normalise_existing_path(
    if (length(arguments) >= 1L) arguments[[1L]] else default_data_path,
    "Dataset"
  )
  output_dir <- normalise_output_path(
    if (length(arguments) >= 2L) arguments[[2L]] else default_output_dir
  )

  data_info <- load_analysis_data(data_path)
  diagnostics <- diagnose_period_models(data_info$data, data_info$periods)
  plot_path <- file.path(
    output_dir,
    "leverage_correction_condition_diagnostics.png"
  )
  create_condition_plot(diagnostics, plot_path)

  period <- diagnostics$period
  total_observations <- sum(period$n)
  h_equal_one <- sum(period$observations_with_h_equal_one)
  full_rank_count <- sum(period$full_rank)
  projection_exact_count <- sum(period$projection_exact)
  normality_rejections <- sum(
    period$shapiro_p_value < SIGNIFICANCE_LEVEL,
    na.rm = TRUE
  )
  homoskedasticity_rejections <- sum(
    period$breusch_pagan_p_value < SIGNIFICANCE_LEVEL,
    na.rm = TRUE
  )

  cat("Leverage-correction condition diagnostics completed successfully\n\n")
  cat("Structural conditions:\n")
  cat(sprintf(
    "  Full-rank period models: %d of %d (condition met)\n",
    full_rank_count,
    nrow(period)
  ))
  cat(sprintf(
    "  Observations with h_ii = 1: %d of %d (required for sigma^2 h_ii = sigma^2)\n",
    h_equal_one,
    total_observations
  ))
  cat(sprintf(
    "  Observed leverage range: %.6f to %.6f; required value: 1\n",
    min(diagnostics$observation$leverage),
    max(diagnostics$observation$leverage)
  ))
  cat(sprintf(
    "  Residual-variance range: %.6f to %.6f (zero-variance exception not met)\n",
    min(period$sigma2_hat),
    max(period$sigma2_hat)
  ))
  cat(sprintf(
    "  Exact second-stage raw-price projections: %d of %d\n",
    projection_exact_count,
    nrow(period)
  ))
  cat("\nSample diagnostics at the 5% threshold:\n")
  cat(sprintf(
    "  Normality rejected in %d of %d periods\n",
    normality_rejections,
    nrow(period)
  ))
  cat(sprintf(
    "  Homoskedasticity rejected in %d of %d periods\n",
    homoskedasticity_rejections,
    nrow(period)
  ))
  cat(paste0(
    "\nConclusion: the equality needed to treat sigma^2 h_ii as the raw-price ",
    "retransformation variance fails for every observation. This conclusion ",
    "does not depend on the normality or homoskedasticity test outcomes.\n"
  ))
  cat("\nPlot: ", normalizePath(plot_path, winslash = "/"), "\n", sep = "")

  invisible(diagnostics)
}

main()

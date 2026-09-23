#!/usr/bin/env Rscript

# ==============================================================================
# Retransformation comparison for every REPS hedonic method except HMTS
# ==============================================================================
#
# This script applies three retransformation specifications to the bundled
# fictitious hedonic_data and creates one two-panel figure for each method:
#
#   1. No retransformation correction (REPS)
#   2. Leverage-based fitted-value variance correction (paper)
#   3. Residual-variance lognormal mean correction
#
# The names describe the statistical mechanism rather than using evaluative
# shorthand. The paper specification is
#
#   p_hat_i = exp(y_hat_i) * exp(sigma_hat^2 * h_ii / 2),
#   beta_back = (X'X)^(-1) X' p_hat,
#
# whereas the conventional homoskedastic lognormal conditional-mean
# specification is
#
#   E(P | X = x) = exp(x' beta_hat + sigma_hat^2 / 2).
#
# Methods covered:
#   - Laspeyres
#   - Paasche
#   - Fisher
#   - Time Dummy
#   - Rolling Time Dummy (four-quarter window)
#   - Repricing (four quarters per year)
#
# For Time Dummy, Rolling Time Dummy, and Repricing, the same model variance
# applies to both sides of each within-model ratio. The lognormal factor
# exp(sigma_hat^2 / 2) therefore cancels exactly. This is a mathematical
# result, not a missing line in the plots.
#
# The script obtains each no-correction series from REPS and independently
# reconstructs it. Execution stops if the two differ by more than 1e-8 index
# points. All alternatives use the same observations, regressors, factor
# levels, period order, and index construction as that method's baseline.
#
# Run from an R terminal or the VS Code "Source R File" command:
#
#   setwd("C:/Users/vgaja/Desktop/REPS")
#   source("research/scripts/compare_retransformation_methods_hedonic_data.R")
#
# Or run from any directory with Rscript:
#
#   Rscript research/scripts/compare_retransformation_methods_hedonic_data.R
#
# Optional arguments:
#
#   Rscript research/scripts/compare_retransformation_methods_hedonic_data.R \
#     path/to/hedonic_data.rda path/to/output_directory
#
# Output: one PNG comparison figure per method.
#
# ============================================================================== 


# 1. Configuration ------------------------------------------------------------

NUMERICAL_VARIABLES <- c("floor_area", "dist_trainstation")
CATEGORICAL_VARIABLES <- c("neighbourhood_code", "dummy_large_city")
PERIOD_VARIABLE <- "period"
DEPENDENT_VARIABLE <- "price"
WINDOW_LENGTH <- 4L
PERIODS_IN_YEAR <- 4L
BASE_INDEX <- 100
BASELINE_TOLERANCE <- 1e-8

METHOD_LABELS <- c(
  laspeyres = "Laspeyres",
  paasche = "Paasche",
  fisher = "Fisher",
  timedummy = "Time Dummy",
  rolling_timedummy = "Rolling Time Dummy",
  repricing = "Repricing"
)

APPROACH_LABELS <- c(
  no_retransformation = "No retransformation correction (REPS)",
  paper_leverage = paste(
    "Leverage-based fitted-value variance correction (paper)"
  ),
  lognormal_residual = "Residual-variance lognormal mean correction"
)


# 2. Paths, validation, and small utilities -----------------------------------

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

  # source() records the current file in a frame's `ofile`. This covers the
  # VS Code "Source R File" command and source("scripts/...") in a console.
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

normalise_levels <- function(levels) {
  if (length(levels) == 0L || any(!is.finite(levels)) || any(levels <= 0)) {
    stop("Index levels must be finite and strictly positive")
  }
  BASE_INDEX * levels / levels[[1L]]
}

safe_plot_range <- function(values, minimum_span = 1) {
  value_range <- range(values, finite = TRUE)
  if (any(!is.finite(value_range))) stop("Cannot plot non-finite values")
  if (diff(value_range) < minimum_span) {
    centre <- mean(value_range)
    value_range <- centre + c(-1, 1) * minimum_span / 2
  }
  value_range
}

fit_semilog_model <- function(X, y, label) {
  n <- nrow(X)
  k <- ncol(X)
  if (n <= k) stop(label, ": n <= k, so residual variance is undefined")

  fit <- stats::lm.fit(x = X, y = y)
  if (fit$rank != k || anyNA(fit$coefficients)) {
    stop(label, ": rank-deficient model matrix")
  }

  beta_hat <- fit$coefficients
  names(beta_hat) <- colnames(X)
  fitted_log_price <- drop(X %*% beta_hat)
  residuals <- y - fitted_log_price
  sigma2_hat <- sum(residuals^2) / (n - k)

  xtx_inverse <- chol2inv(chol(crossprod(X)))
  leverage <- rowSums((X %*% xtx_inverse) * X)
  if (abs(sum(leverage) - k) > 1e-7) {
    stop(label, ": leverage trace check failed")
  }

  paper_prices <- exp(fitted_log_price) * exp(sigma2_hat * leverage / 2)
  beta_back_paper <- drop(
    xtx_inverse %*% crossprod(X, paper_prices)
  )
  names(beta_back_paper) <- colnames(X)

  list(
    beta_hat = beta_hat,
    beta_back_paper = beta_back_paper,
    sigma2_hat = sigma2_hat,
    leverage = leverage,
    n = n,
    k = k
  )
}

standard_result <- function(periods, no_correction, paper, lognormal,
                            residual_variance, mean_leverage, max_leverage) {
  data.frame(
    period = periods,
    independent_no_retransformation_check = as.numeric(no_correction),
    index_paper_leverage_correction = as.numeric(paper),
    index_lognormal_residual_correction = as.numeric(lognormal),
    residual_variance = as.numeric(residual_variance),
    mean_leverage = as.numeric(mean_leverage),
    max_leverage = as.numeric(max_leverage),
    stringsAsFactors = FALSE
  )
}


# 3. Load one common analysis sample ------------------------------------------

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
  complete_rows <- stats::complete.cases(analysis_data)
  removed_rows <- sum(!complete_rows)
  analysis_data <- analysis_data[complete_rows, , drop = FALSE]

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

  periods <- sort(unique(analysis_data[[PERIOD_VARIABLE]]))
  if (length(periods) < WINDOW_LENGTH) {
    stop("At least ", WINDOW_LENGTH, " periods are required")
  }

  list(
    raw_data = raw_data,
    analysis_data = analysis_data,
    periods = periods,
    removed_rows = removed_rows
  )
}


# 4. Period-specific models: Laspeyres, Paasche, and Fisher -------------------

build_period_model_components <- function(analysis_data, periods) {
  quality_formula <- stats::reformulate(
    c(NUMERICAL_VARIABLES, CATEGORICAL_VARIABLES),
    intercept = TRUE
  )
  X_all <- stats::model.matrix(quality_formula, data = analysis_data)
  y_all <- log(analysis_data[[DEPENDENT_VARIABLE]])

  beta_hat <- matrix(
    NA_real_, nrow = length(periods), ncol = ncol(X_all),
    dimnames = list(periods, colnames(X_all))
  )
  beta_back_paper <- beta_hat
  z_bar <- beta_hat
  sigma2 <- mean_h <- max_h <- numeric(length(periods))

  for (i in seq_along(periods)) {
    rows <- analysis_data[[PERIOD_VARIABLE]] == periods[[i]]
    X <- X_all[rows, , drop = FALSE]
    model <- fit_semilog_model(X, y_all[rows], paste("Period", periods[[i]]))
    beta_hat[i, ] <- model$beta_hat
    beta_back_paper[i, ] <- model$beta_back_paper
    z_bar[i, ] <- colMeans(X)
    sigma2[[i]] <- model$sigma2_hat
    mean_h[[i]] <- mean(model$leverage)
    max_h[[i]] <- max(model$leverage)
  }

  list(
    beta_hat = beta_hat,
    beta_back_paper = beta_back_paper,
    z_bar = z_bar,
    sigma2 = sigma2,
    mean_h = mean_h,
    max_h = max_h,
    design_columns = colnames(X_all)
  )
}

calculate_double_imputation_methods <- function(components, periods) {
  beta <- components$beta_hat
  beta_paper <- components$beta_back_paper
  z <- components$z_bar
  s2 <- components$sigma2
  z0 <- z[1L, ]
  beta0 <- beta[1L, ]
  beta0_paper <- beta_paper[1L, ]

  las_levels_no <- exp(drop(beta %*% z0))
  las_levels_paper <- drop(beta_paper %*% z0)
  las_levels_lognormal <- exp(drop(beta %*% z0) + s2 / 2)

  laspeyres <- standard_result(
    periods,
    normalise_levels(las_levels_no),
    normalise_levels(las_levels_paper),
    normalise_levels(las_levels_lognormal),
    s2,
    components$mean_h,
    components$max_h
  )

  current_no <- exp(rowSums(z * beta))
  base_no <- exp(drop(z %*% beta0))
  current_paper <- rowSums(z * beta_paper)
  base_paper <- drop(z %*% beta0_paper)
  current_lognormal <- exp(rowSums(z * beta) + s2 / 2)
  base_lognormal <- exp(drop(z %*% beta0) + s2[[1L]] / 2)

  paasche <- standard_result(
    periods,
    BASE_INDEX * current_no / base_no,
    BASE_INDEX * current_paper / base_paper,
    BASE_INDEX * current_lognormal / base_lognormal,
    s2,
    components$mean_h,
    components$max_h
  )

  fisher <- standard_result(
    periods,
    sqrt(
      laspeyres$independent_no_retransformation_check *
        paasche$independent_no_retransformation_check
    ),
    sqrt(
      laspeyres$index_paper_leverage_correction *
        paasche$index_paper_leverage_correction
    ),
    sqrt(
      laspeyres$index_lognormal_residual_correction *
        paasche$index_lognormal_residual_correction
    ),
    s2,
    components$mean_h,
    components$max_h
  )

  list(laspeyres = laspeyres, paasche = paasche, fisher = fisher)
}


# 5. Time Dummy and Rolling Time Dummy ----------------------------------------

fit_time_dummy_window <- function(window_data, window_periods, label) {
  window_data <- window_data
  window_data[[PERIOD_VARIABLE]] <- factor(
    window_data[[PERIOD_VARIABLE]],
    levels = window_periods
  )
  model_formula <- stats::reformulate(
    c(NUMERICAL_VARIABLES, CATEGORICAL_VARIABLES, PERIOD_VARIABLE),
    intercept = TRUE
  )
  X <- stats::model.matrix(model_formula, data = window_data)
  y <- log(window_data[[DEPENDENT_VARIABLE]])
  model <- fit_semilog_model(X, y, label)

  model_terms <- stats::terms(model_formula)
  period_term <- match(PERIOD_VARIABLE, attr(model_terms, "term.labels"))
  assignment <- attr(X, "assign")
  period_columns <- assignment == period_term
  quality_columns <- !period_columns
  common_quality <- colMeans(X[, quality_columns, drop = FALSE])

  no_levels <- paper_levels <- lognormal_levels <- numeric(length(window_periods))
  for (i in seq_along(window_periods)) {
    rows <- window_data[[PERIOD_VARIABLE]] == window_periods[[i]]
    evaluation_vector <- numeric(ncol(X))
    evaluation_vector[quality_columns] <- common_quality
    if (any(period_columns)) {
      evaluation_vector[period_columns] <- colMeans(
        X[rows, period_columns, drop = FALSE]
      )
    }

    linear_predictor <- sum(evaluation_vector * model$beta_hat)
    no_levels[[i]] <- exp(linear_predictor)
    paper_levels[[i]] <- sum(evaluation_vector * model$beta_back_paper)
    lognormal_levels[[i]] <- exp(linear_predictor + model$sigma2_hat / 2)
  }

  if (any(paper_levels <= 0)) {
    stop(label, ": paper correction produced a non-positive fitted level")
  }

  list(
    no = no_levels,
    paper = paper_levels,
    lognormal = lognormal_levels,
    sigma2 = model$sigma2_hat,
    mean_h = mean(model$leverage),
    max_h = max(model$leverage)
  )
}

calculate_time_dummy_method <- function(analysis_data, periods) {
  model <- fit_time_dummy_window(
    analysis_data,
    periods,
    "Pooled Time Dummy model"
  )
  count <- length(periods)
  standard_result(
    periods,
    normalise_levels(model$no),
    normalise_levels(model$paper),
    normalise_levels(model$lognormal),
    rep(model$sigma2, count),
    rep(model$mean_h, count),
    rep(model$max_h, count)
  )
}

calculate_rolling_time_dummy_method <- function(analysis_data, periods) {
  approaches <- c("no", "paper", "lognormal")
  growth <- matrix(
    NA_real_, nrow = length(periods), ncol = length(approaches),
    dimnames = list(periods, approaches)
  )
  sigma2 <- mean_h <- max_h <- rep(NA_real_, length(periods))
  last_window_start <- length(periods) - WINDOW_LENGTH + 1L

  for (start in seq_len(last_window_start)) {
    window_periods <- periods[start:(start + WINDOW_LENGTH - 1L)]
    rows <- analysis_data[[PERIOD_VARIABLE]] %in% window_periods
    model <- fit_time_dummy_window(
      analysis_data[rows, , drop = FALSE],
      window_periods,
      paste0("Rolling Time Dummy window ending ", tail(window_periods, 1L))
    )
    local_growth <- cbind(
      no = model$no / c(NA_real_, head(model$no, -1L)),
      paper = model$paper / c(NA_real_, head(model$paper, -1L)),
      lognormal = model$lognormal / c(NA_real_, head(model$lognormal, -1L))
    )
    local_growth[1L, ] <- 1

    if (start == 1L) {
      target <- seq_len(WINDOW_LENGTH)
      growth[target, ] <- local_growth
      sigma2[target] <- model$sigma2
      mean_h[target] <- model$mean_h
      max_h[target] <- model$max_h
    } else {
      target <- start + WINDOW_LENGTH - 1L
      growth[target, ] <- local_growth[WINDOW_LENGTH, ]
      sigma2[target] <- model$sigma2
      mean_h[target] <- model$mean_h
      max_h[target] <- model$max_h
    }
  }

  if (anyNA(growth) || any(growth <= 0)) {
    stop("Rolling Time Dummy growth-rate construction failed")
  }

  standard_result(
    periods,
    BASE_INDEX * cumprod(growth[, "no"]),
    BASE_INDEX * cumprod(growth[, "paper"]),
    BASE_INDEX * cumprod(growth[, "lognormal"]),
    sigma2,
    mean_h,
    max_h
  )
}


# 6. Repricing ---------------------------------------------------------------

calculate_repricing_method <- function(analysis_data, periods) {
  quality_formula <- stats::reformulate(
    c(NUMERICAL_VARIABLES, CATEGORICAL_VARIABLES),
    intercept = TRUE
  )
  X_all <- stats::model.matrix(quality_formula, data = analysis_data)
  y_all <- log(analysis_data[[DEPENDENT_VARIABLE]])
  base_year_periods <- periods[seq_len(PERIODS_IN_YEAR)]
  base_rows <- analysis_data[[PERIOD_VARIABLE]] %in% base_year_periods
  model <- fit_semilog_model(
    X_all[base_rows, , drop = FALSE],
    y_all[base_rows],
    "Repricing base-year model"
  )

  z_bar <- t(vapply(
    periods,
    function(period) {
      colMeans(X_all[analysis_data[[PERIOD_VARIABLE]] == period, , drop = FALSE])
    },
    numeric(ncol(X_all))
  ))
  observed_geometric_mean <- vapply(
    periods,
    function(period) {
      exp(mean(y_all[analysis_data[[PERIOD_VARIABLE]] == period]))
    },
    numeric(1L)
  )

  predicted_no <- exp(drop(z_bar %*% model$beta_hat))
  predicted_paper <- drop(z_bar %*% model$beta_back_paper)
  predicted_lognormal <- exp(
    drop(z_bar %*% model$beta_hat) + model$sigma2_hat / 2
  )
  if (any(predicted_paper <= 0)) {
    stop("Repricing paper correction produced a non-positive fitted level")
  }

  make_repricing_index <- function(predicted_level) {
    BASE_INDEX *
      (observed_geometric_mean / observed_geometric_mean[[1L]]) /
      (predicted_level / predicted_level[[1L]])
  }

  count <- length(periods)
  standard_result(
    periods,
    make_repricing_index(predicted_no),
    make_repricing_index(predicted_paper),
    make_repricing_index(predicted_lognormal),
    rep(model$sigma2_hat, count),
    rep(mean(model$leverage), count),
    rep(max(model$leverage), count)
  )
}


# 7. REPS baselines and strict agreement checks -------------------------------

calculate_reps_baselines <- function(raw_data, periods) {
  baselines <- list()
  for (method in names(METHOD_LABELS)) {
    arguments <- list(
      dataset = raw_data,
      method = method,
      period_variable = PERIOD_VARIABLE,
      dependent_variable = DEPENDENT_VARIABLE,
      numerical_variables = NUMERICAL_VARIABLES,
      categorical_variables = CATEGORICAL_VARIABLES,
      number_of_observations = FALSE
    )
    if (method == "rolling_timedummy") arguments$window_length <- WINDOW_LENGTH
    if (method == "repricing") arguments$periods_in_year <- PERIODS_IN_YEAR

    result <- do.call(REPS::calculate_hedonic_index, arguments)
    positions <- match(periods, as.character(result[[PERIOD_VARIABLE]]))
    if (anyNA(positions)) stop("REPS ", method, " output omits analysis periods")
    baselines[[method]] <- as.numeric(result$Index[positions])
  }
  baselines
}

attach_reps_baselines <- function(independent_results, reps_baselines) {
  checked <- list()
  baseline_differences <- numeric(length(independent_results))
  names(baseline_differences) <- names(independent_results)

  for (method in names(independent_results)) {
    result <- independent_results[[method]]
    difference <- max(abs(
      result$independent_no_retransformation_check - reps_baselines[[method]]
    ))
    if (!is.finite(difference) || difference > BASELINE_TOLERANCE) {
      stop(
        METHOD_LABELS[[method]],
        " independent baseline does not reproduce REPS. Maximum difference: ",
        format(difference, scientific = TRUE)
      )
    }

    result$index_no_retransformation_reps <- reps_baselines[[method]]
    result$paper_minus_reps <-
      result$index_paper_leverage_correction -
      result$index_no_retransformation_reps
    result$lognormal_minus_reps <-
      result$index_lognormal_residual_correction -
      result$index_no_retransformation_reps

    base_values <- unlist(result[1L, c(
      "index_no_retransformation_reps",
      "index_paper_leverage_correction",
      "index_lognormal_residual_correction"
    )])
    if (max(abs(base_values - BASE_INDEX)) > 1e-9) {
      stop(METHOD_LABELS[[method]], " is not normalized to the common base")
    }

    result$method <- method
    result$method_label <- unname(METHOD_LABELS[[method]])
    checked[[method]] <- result
    baseline_differences[[method]] <- difference
  }

  list(results = checked, baseline_differences = baseline_differences)
}


# 8. One comparable plot per method ------------------------------------------

create_method_plot <- function(result, method, base_period, plot_path) {
  x <- seq_len(nrow(result))
  colours <- c(
    no_retransformation = "#202020",
    paper_leverage = "#C0392B",
    lognormal_residual = "#1F618D"
  )
  all_indices <- c(
    result$index_no_retransformation_reps,
    result$index_paper_leverage_correction,
    result$index_lognormal_residual_correction
  )
  differences <- c(result$paper_minus_reps, result$lognormal_minus_reps, 0)

  grDevices::png(plot_path, width = 2400, height = 1550, res = 200)
  old_par <- graphics::par(
    mfrow = c(2, 1),
    mar = c(3.2, 5.4, 3.3, 1.5),
    oma = c(3.7, 0, 2.6, 0),
    las = 1
  )
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)

  # Draw the REPS baseline last and add markers so it remains visible when the
  # lognormal series coincides with it.
  graphics::plot(
    x,
    result$index_lognormal_residual_correction,
    type = "l",
    lwd = 3,
    col = colours[["lognormal_residual"]],
    ylim = safe_plot_range(all_indices),
    xaxt = "n",
    xlab = "",
    ylab = paste0(METHOD_LABELS[[method]], " index (base = 100)"),
    main = "A. Index levels"
  )
  graphics::lines(
    x,
    result$index_paper_leverage_correction,
    lwd = 3,
    col = colours[["paper_leverage"]]
  )
  graphics::lines(
    x,
    result$index_no_retransformation_reps,
    lwd = 3.5,
    lty = 2,
    col = colours[["no_retransformation"]]
  )
  graphics::points(
    x,
    result$index_no_retransformation_reps,
    pch = 1,
    cex = 0.55,
    lwd = 1.2,
    col = colours[["no_retransformation"]]
  )
  graphics::abline(h = BASE_INDEX, col = "#BBBBBB", lty = 3)
  graphics::legend(
    "topleft",
    legend = unname(APPROACH_LABELS),
    col = unname(colours),
    lty = c(2, 1, 1),
    lwd = c(3.5, 3, 3),
    pch = c(1, NA, NA),
    bty = "n",
    cex = 0.78
  )

  graphics::plot(
    x,
    result$paper_minus_reps,
    type = "l",
    lwd = 3,
    col = colours[["paper_leverage"]],
    ylim = safe_plot_range(differences, minimum_span = 0.1),
    xaxt = "n",
    xlab = "",
    ylab = "Difference from REPS baseline\n(index points)",
    main = "B. Retransformation effect"
  )
  graphics::lines(
    x,
    result$lognormal_minus_reps,
    lwd = 3,
    col = colours[["lognormal_residual"]]
  )
  graphics::abline(h = 0, col = colours[["no_retransformation"]], lty = 2, lwd = 2)
  graphics::legend(
    "topleft",
    legend = c(
      "Leverage-based correction minus REPS",
      "Lognormal mean correction minus REPS"
    ),
    col = c(colours[["paper_leverage"]], colours[["lognormal_residual"]]),
    lty = 1,
    lwd = 3,
    bty = "n",
    cex = 0.82
  )

  axis_positions <- seq.int(1L, nrow(result), by = 4L)
  graphics::axis(
    side = 1,
    at = axis_positions,
    labels = substr(result$period[axis_positions], 1L, 4L),
    las = 2,
    cex.axis = 0.85
  )
  graphics::mtext("Period", side = 1, outer = TRUE, line = 1.6)
  graphics::mtext(
    sprintf(
      "Hedonic data: %s under three retransformation specifications (%s = 100)",
      METHOD_LABELS[[method]],
      base_period
    ),
    outer = TRUE,
    cex = 1.08,
    font = 2
  )

  invisible(plot_path)
}


# 9. Concise numerical summary ------------------------------------------------

make_method_summary <- function(results, baseline_differences) {
  summaries <- lapply(names(results), function(method) {
    result <- results[[method]]
    paper_position <- which.max(abs(result$paper_minus_reps))
    lognormal_position <- which.max(abs(result$lognormal_minus_reps))
    data.frame(
      method = method,
      method_label = unname(METHOD_LABELS[[method]]),
      maximum_baseline_check_difference = baseline_differences[[method]],
      maximum_absolute_paper_difference = abs(
        result$paper_minus_reps[[paper_position]]
      ),
      paper_maximum_period = result$period[[paper_position]],
      maximum_absolute_lognormal_difference = abs(
        result$lognormal_minus_reps[[lognormal_position]]
      ),
      lognormal_maximum_period = result$period[[lognormal_position]],
      final_reps_index = tail(result$index_no_retransformation_reps, 1L),
      final_paper_index = tail(result$index_paper_leverage_correction, 1L),
      final_lognormal_index = tail(
        result$index_lognormal_residual_correction, 1L
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, summaries)
}

# 10. Main analysis -----------------------------------------------------------

main <- function() {
  called_by_rscript <- length(grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )) == 1L
  command_line_arguments <- if (called_by_rscript) {
    commandArgs(trailingOnly = TRUE)
  } else {
    character(0)
  }
  repository_root <- find_repository_root()
  default_data_path <- file.path(repository_root, "data", "hedonic_data.rda")
  default_output_dir <- file.path(
    repository_root,
    "research",
    "figures",
    "empirical"
  )
  data_path <- normalise_existing_path(
    if (length(command_line_arguments) >= 1L) {
      command_line_arguments[[1L]]
    } else {
      default_data_path
    },
    "Dataset"
  )
  output_dir <- normalise_output_path(
    if (length(command_line_arguments) >= 2L) {
      command_line_arguments[[2L]]
    } else {
      default_output_dir
    }
  )

  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The pkgload package is required to load the local REPS source tree")
  }
  pkgload::load_all(repository_root, quiet = TRUE)

  data_info <- load_analysis_data(data_path)
  components <- build_period_model_components(
    data_info$analysis_data,
    data_info$periods
  )
  independent_results <- calculate_double_imputation_methods(
    components,
    data_info$periods
  )
  independent_results$timedummy <- calculate_time_dummy_method(
    data_info$analysis_data,
    data_info$periods
  )
  independent_results$rolling_timedummy <- calculate_rolling_time_dummy_method(
    data_info$analysis_data,
    data_info$periods
  )
  independent_results$repricing <- calculate_repricing_method(
    data_info$analysis_data,
    data_info$periods
  )

  # Enforce the presentation order used in METHOD_LABELS.
  independent_results <- independent_results[names(METHOD_LABELS)]
  reps_baselines <- calculate_reps_baselines(
    data_info$raw_data,
    data_info$periods
  )
  checked <- attach_reps_baselines(independent_results, reps_baselines)
  results <- checked$results
  summary_table <- make_method_summary(
    results,
    checked$baseline_differences
  )

  plot_paths <- character(length(results))
  names(plot_paths) <- names(results)
  for (method in names(results)) {
    plot_paths[[method]] <- file.path(
      output_dir,
      paste0(method, "_retransformation_comparison.png")
    )
    create_method_plot(
      results[[method]],
      method,
      data_info$periods[[1L]],
      plot_paths[[method]]
    )
  }

  cat("All non-HMTS hedonic method comparisons completed successfully\n\n")
  print(
    summary_table[, c(
      "method_label",
      "maximum_baseline_check_difference",
      "maximum_absolute_paper_difference",
      "maximum_absolute_lognormal_difference"
    )],
    row.names = FALSE,
    digits = 7
  )
  cat("\nApproach names:\n")
  for (label in unname(APPROACH_LABELS)) cat("  - ", label, "\n", sep = "")
  cat("\nPlots:\n")
  for (method in names(plot_paths)) {
    cat(
      "  ", METHOD_LABELS[[method]], ": ",
      normalizePath(plot_paths[[method]], winslash = "/"), "\n",
      sep = ""
    )
  }
  invisible(results)
}

main()

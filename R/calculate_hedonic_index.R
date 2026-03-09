#' Calculate index based on specified method (Fisher, Laspeyres, Paasche, HMTS, Time Dummy, Rolling Time Dummy)
#'
#' Central hub function to calculate index figures using different methods. Can also calculate chained indices using the Annual Overlap Method.
#'
#' @author Vivek Gajadhar
#' @param method One of: "fisher", "laspeyres", "paasche", "hmts", "timedummy", "rolling_timedummy", "repricing"
#' @param dataset Data frame with input data
#' @param period_variable A string with the name of the column containing time periods. 
#' @param dependent_variable Usually the price
#' @param numerical_variables Vector with numeric quality-determining variables
#' @param categorical_variables Vector with categorical variables (also dummies)
#' @param reference_period Period or group of periods that will be set to 100
#' @param number_of_observations Logical, whether to show number of observations (default = TRUE)
#' @param chained Logical. If TRUE, calculates a chained index using the Annual Overlap Method. Default is FALSE.
#' @param ... Additional method-specific arguments passed to the underlying functions:
#' \itemize{
#'   \item \code{periods_in_year}: (Required for HMTS/Repricing) Number of periods per year (e.g. 12 for months)
#'   \item \code{number_preliminary_periods}: (Required for HMTS) Number of preliminary periods
#'   \item \code{production_since}: (Optional for HMTS) Start period for production simulation. Default = NULL
#'   \item \code{resting_points}: (Optional for HMTS) Whether to return detailed outputs. Default = FALSE
#'   \item \code{imputation}: (Optional for Laspeyres/Paasche) Include imputation values? Default = FALSE
#'   \item \code{window_length}: (Required for Rolling Time Dummy) Window size in number of periods
#' }
#'
#' @return A data.frame (or list for HMTS with resting_points = TRUE; or named list if multiple methods are used)
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom utils tail
calculate_hedonic_index <- function(dataset,
                                  method,
                                  period_variable,
                                  dependent_variable,
                                  numerical_variables = NULL,
                                  categorical_variables = NULL,
                                  reference_period = NULL,
                                  number_of_observations = TRUE,
                                  chained = FALSE,
                                  ...) {
  
  # Prevents call of false methods
  method <- tolower(method)
  valid_methods <- c("fisher", "laspeyres", "paasche", "hmts", "timedummy", "rolling_timedummy", "repricing")
  invalid_methods <- setdiff(method, valid_methods)
  if (length(invalid_methods) > 0) {
    stop(paste0("Invalid method(s): ", paste(invalid_methods, collapse = ", "),
                ". Please choose from: ", paste(valid_methods, collapse = ", "), "."))
  }
  
  extra_args <- list(...)
  
  # Prevent resting_points = TRUE in multi-method context
  if (length(method) > 1 && isTRUE(extra_args$resting_points)) {
    stop("Using 'resting_points = TRUE' is only allowed with a single method ('hmts').")
  }
  
  validate_input(dataset, period_variable, dependent_variable, numerical_variables, categorical_variables)
  
  # ============================================================================
  # INTERNAL CALCULATION ENGINE & GATEKEEPER
  # ============================================================================
  run_method <- function(m, target_dataset, target_reference_period) {
    
    func_map <- list(
      fisher = "calculate_fisher",
      laspeyres = "calculate_laspeyres",
      paasche = "calculate_paasche",
      hmts = "calculate_hmts",
      timedummy = "calculate_time_dummy",
      rolling_timedummy = "calculate_rolling_timedummy",
      repricing = "calculate_repricing"
    )
    
    target_func <- func_map[[m]]
    
    base_args <- list(
      dataset = target_dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      numerical_variables = numerical_variables,
      categorical_variables = categorical_variables,
      reference_period = target_reference_period,
      number_of_observations = number_of_observations
    )
    
    accepted_args <- names(formals(target_func))
    valid_extra_args <- extra_args[names(extra_args) %in% accepted_args]
    
    # --- Gatekeeper ---
    if (m == "rolling_timedummy") {
      if (!("window_length" %in% names(valid_extra_args))) stop("Validation Error: You must specify 'window_length' (e.g., window_length = 5) for the 'rolling_timedummy' method.")
    }
    if (m == "hmts") {
      if (!("periods_in_year" %in% names(valid_extra_args))) stop("Validation Error: You must specify 'periods_in_year' (e.g., periods_in_year = 4) for the 'hmts' method.")
      if (!("number_preliminary_periods" %in% names(valid_extra_args))) stop("Validation Error: You must specify 'number_preliminary_periods' for the 'hmts' method.")
      if (!("production_since" %in% names(valid_extra_args))) valid_extra_args$production_since <- NULL
      if (!("resting_points" %in% names(valid_extra_args))) valid_extra_args$resting_points <- FALSE
    }
    if (m == "repricing") {
      if (!("periods_in_year" %in% names(valid_extra_args))) stop("Validation Error: You must specify 'periods_in_year' for the 'repricing' method.")
    }
    if (m %in% c("laspeyres", "paasche")) {
      if (!("imputation" %in% names(valid_extra_args))) valid_extra_args$imputation <- FALSE
    }
    
    final_args <- c(base_args, valid_extra_args)
    return(do.call(target_func, final_args))
  }
  
  # ============================================================================
  # PROCESSING LOGIC (CHAINED vs UNCHAINED)
  # ============================================================================
  process_single_method <- function(m) {
    
    # Option 1: Standard calculation (Unchained)
    if (!isTRUE(chained)) {
      return(run_method(m, dataset, reference_period))
    }
    
    # Option 2: Chained calculation (Annual Overlap)
    periods_raw <- as.character(dataset[[period_variable]])
    unique_periods <- sort(unique(periods_raw))
    
    get_year <- function(p) as.integer(substr(p, 1, 4))
    years <- unique(get_year(unique_periods))
    
    short_term_results <- list()
    
    # Year 1
    first_year <- min(years)
    first_year_periods <- unique_periods[get_year(unique_periods) == first_year]
    data_subset_first <- dataset[dataset[[period_variable]] %in% first_year_periods, ]
    
    short_term_results[[as.character(first_year)]] <- run_method(m, data_subset_first, NULL)
    
    # Year 2+
    if (length(years) > 1) {
      for (i in 2:length(years)) {
        current_year <- years[i]
        prev_year <- years[i-1]
        
        current_periods <- unique_periods[get_year(unique_periods) == current_year]
        prev_periods <- unique_periods[get_year(unique_periods) == prev_year]
        overlap_period <- tail(sort(prev_periods), 1)
        
        calculation_periods <- c(overlap_period, current_periods)
        data_subset <- dataset[dataset[[period_variable]] %in% calculation_periods, ]
        
        index_current <- run_method(m, data_subset, overlap_period)
        index_current_clean <- index_current[index_current$period != overlap_period, ]
        
        short_term_results[[as.character(current_year)]] <- index_current_clean
      }
    }
    
    # Bind everything together
    full_series <- dplyr::bind_rows(short_term_results)
    full_series <- full_series[order(full_series$period), ]
    
    final_index <- numeric(nrow(full_series))
    periods_vec <- full_series$period
    
    n_y1 <- length(first_year_periods)
    final_index[1:n_y1] <- short_term_results[[1]]$Index / short_term_results[[1]]$Index[1] * 100
    
    current_idx <- n_y1 + 1
    if (length(short_term_results) > 1) {
      for (i in 2:length(short_term_results)) {
        factors <- short_term_results[[i]]$Index / 100
        previous_level <- final_index[current_idx - 1]
        
        n_obs <- length(factors)
        final_index[current_idx:(current_idx + n_obs - 1)] <- factors * previous_level
        current_idx <- current_idx + n_obs
      }
    }
    
    result_table <- data.frame(period = periods_vec, Index = final_index)
    
    # Re-reference the chained index to the target year
    if (!is.null(reference_period)) {
      result_table$Index <- calculate_index(result_table$period, result_table$Index, reference_period)
    }
    
    return(result_table)
  }
  
  # ============================================================================
  # OUTPUT (Single or Multi Method)
  # ============================================================================
  if (length(method) == 1) {
    return(process_single_method(method))
  }
  
  result_list <- lapply(method, process_single_method)
  names(result_list) <- method
  return(result_list)
}

#' Plot index output from calculate_hedonic_index 
#'
#' Static price index plot using base R graphics with grid lines and external legend.
#'
#' Supports both single index data.frame and named list of multiple methods.
#' X-axis shows only first period of each year with rotated labels to avoid clutter.
#'
#' @author Vivek Gajadhar
#' @param index_output A data.frame or named list of data.frames (from calculate_hedonic_index())
#' @param title Optional plot title
#' @return None. Draws plots in the active graphics device.
#' @importFrom graphics axis grid legend lines par plot text
#' @export
plot_price_index <- function(index_output, title = NULL) {
  
  op <- par(mfrow = c(1, 1))
  on.exit(par(op))
  
  # Helper to extract first period of each year
  get_year_start_periods <- function(periods) {
    years <- substr(periods, 1, 4)
    periods[!duplicated(years)]
  }
  
  # Colorblind-friendly palette (Okabe-Ito)
  cb_palette <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
  )
  
  if (is.null(title)) {
    if (is.data.frame(index_output)) {
      title <- "Price Index"
    } else if (is.list(index_output)) {
      title <- "Price Index Comparison"
    }
  }
  
  if (is.data.frame(index_output)) {
    # Single index
    df <- index_output[order(index_output$period), ]
    periods <- as.factor(df$period)
    period_levels <- levels(periods)
    x <- 1:length(period_levels)
    breaks <- get_year_start_periods(period_levels)
    break_indices <- match(breaks, period_levels)
    
    plot(x, df$Index, type = "n",
         xaxt = "n", xlab = "", ylab = "Index",
         main = title)
    grid(col = "grey90", lty = "dotted")
    lines(x, df$Index, type = "b", pch = 19, col = cb_palette[1])
    axis(1, at = break_indices, labels = FALSE)
    text(
      x = break_indices,
      y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
      labels = breaks,
      srt = 45,
      adj = 1,
      xpd = TRUE,
      cex = 0.8
    )
  } else if (is.list(index_output)) {
    # Multiple methods
    combined <- do.call(rbind, lapply(names(index_output), function(name) {
      df <- index_output[[name]]
      df <- df[order(df$period), ]
      df$method <- name
      return(df)
    }))
    combined$period <- as.factor(combined$period)
    period_levels <- levels(combined$period)
    x <- 1:length(period_levels)
    breaks <- get_year_start_periods(period_levels)
    break_indices <- match(breaks, period_levels)
    
    # Global y range
    y_range <- range(combined$Index, na.rm = TRUE)
    
    # Empty plot first
    plot(NA, xlim = range(x), ylim = y_range,
         xaxt = "n", xlab = "", ylab = "Index",
         main = title)
    grid(col = "grey90", lty = "dotted")
    
    # Plot each method
    methods <- unique(combined$method)
    for (i in seq_along(methods)) {
      method_name <- methods[i]
      df <- combined[combined$method == method_name, ]
      df <- df[order(df$period), ]
      lines(x, df$Index, type = "b", pch = 19,
            col = cb_palette[(i - 1) %% length(cb_palette) + 1])
    }
    
    # X-axis with rotated labels
    axis(1, at = break_indices, labels = FALSE)
    text(
      x = break_indices,
      y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]),
      labels = breaks,
      srt = 45,
      adj = 1,
      xpd = TRUE,
      cex = 0.8
    )
    
    # Legend outside the plot
    legend("bottomright",
           legend = methods,
           col = cb_palette[seq_along(methods)],
           pch = 19, lty = 1,
           bty = "n",
           xpd = TRUE)
    
  } else {
    stop("Unsupported input type: must be a data.frame or named list of data.frames from calculate_hedonic_index()")
  }
}
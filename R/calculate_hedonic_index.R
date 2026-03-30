#' Calculate index based on specified method (Fisher, Laspeyres, Paasche, HMTS, Time Dummy, Rolling Time Dummy)
#'
#' Central hub function to calculate index figures using different methods.
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
#' @param ... Additional method-specific arguments passed to the underlying functions:
#' \itemize{
#'   \item \code{periods_in_year}: (Required for Repricing) Number of periods per year (e.g. 12 for months, 4 for quarters)
#'   \item \code{number_preliminary_periods}: (Required for HMTS) Number of preliminary periods
#'   \item \code{production_since}: (Optional for HMTS) Start period for production simulation. Default = NULL
#'   \item \code{resting_points}: (Optional for HMTS) Whether to return detailed outputs. Default = FALSE
#'   \item \code{imputation}: (Optional for Laspeyres/Paasche) Include imputation values? Default = FALSE
#'   \item \code{window_length}: (Required for Rolling Time Dummy) Window size in number of periods
#' }
#'
#' @return A data.frame (or list for HMTS with resting_points = TRUE; or named list if multiple methods are used)
#' @export
#' @examples
#' \dontrun{
#' data("data_constraxion")
#' 
#' Tbl_indices <- REPS::calculate_hedonic_index(
#'   method = c("fisher", "hmts", "laspeyres", "paasche",
#'  "repricing", "timedummy", "rolling_timedummy"),
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   numerical_variables = c("floor_area", "dist_trainstation"),
#'   categorical_variables = c("neighbourhood_code", "dummy_large_city"),
#'   reference_period = "2015",
#'   number_of_observations = FALSE,
#'   periods_in_year = 4,
#'   number_preliminary_periods = 1,
#'   window_length = 4,
#'   production_since = NULL,
#'   resting_points = FALSE,
#'   imputation = FALSE
#' )
#' }
calculate_hedonic_index <- function(dataset,
                                  method,
                                  period_variable,
                                  dependent_variable,
                                  numerical_variables = NULL,
                                  categorical_variables = NULL,
                                  reference_period = NULL,
                                  number_of_observations = TRUE,
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
  
  # Dynamic dispatch function
  run_method <- function(m) {
    
    # Map the string to your actual package functions
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
    
    # Bundle the core arguments that EVERY method uses
    base_args <- list(
      dataset = dataset,
      period_variable = period_variable,
      dependent_variable = dependent_variable,
      numerical_variables = numerical_variables,
      categorical_variables = categorical_variables,
      reference_period = reference_period,
      number_of_observations = number_of_observations
    )
    
    # Filter the `...` arguments to only pass what the target function accepts
    accepted_args <- names(formals(target_func))
    valid_extra_args <- extra_args[names(extra_args) %in% accepted_args]
    
    # ==========================================================================
    # GATEKEEPER FOR EXTRA PARAMETERS
    # ==========================================================================
    
    # 1. Rolling Time Dummy Gate
    if (m == "rolling_timedummy") {
      if (!("window_length" %in% names(valid_extra_args))) {
        valid_extra_args$window_length <- 5
        message("Note: 'window_length' was not specified. A default value of 5 has been applied.")
      }
    }
    
    # 2. HMTS Gate
    if (m == "hmts") {
      if (!("number_preliminary_periods" %in% names(valid_extra_args))) {
        valid_extra_args$number_preliminary_periods <- 3
        message("Note: 'number_preliminary_periods' was not specified. A default value of 3 has been applied.")
      }
      
      
      if (!("production_since" %in% names(valid_extra_args))) {
        valid_extra_args$production_since <- NULL
        message("Note: 'production since' was not specified. A default value of NULL has been applied. Enter the initial production period to establish a definitive timeline for all future calculations.")
      }

      if (!("resting_points" %in% names(valid_extra_args))) valid_extra_args$resting_points <- FALSE
    }
    
    # 3. Repricing Gate
    if (m == "repricing") {
      if (!("periods_in_year" %in% names(valid_extra_args))) stop("Validation Error: You must specify 'periods_in_year' for the 'repricing' method.")
    }
    
    # 4. Laspeyres & Paasche Gate
    if (m %in% c("laspeyres", "paasche")) {
      # Inject default for optional imputation parameter if missing
      if (!("imputation" %in% names(valid_extra_args))) valid_extra_args$imputation <- FALSE
    }
    
    # ==========================================================================
    
    # Execute the underlying function with the safely validated combined arguments
    final_args <- c(base_args, valid_extra_args)
    return(do.call(target_func, final_args))
  }
  
  # Single method: return output directly
  if (length(method) == 1) {
    return(run_method(method))
  }
  
  # Multiple methods: return named list
  result_list <- lapply(method, run_method)
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
    
    # ==========================================================================
    # GATEKEEPER FOR RESTING POINTS
    # ==========================================================================
    # A valid multi-method list contains ONLY dataframes that have 'period' and 'Index'
    is_valid_multi <- all(sapply(index_output, function(x) {
      is.data.frame(x) && "period" %in% names(x) && "Index" %in% names(x)
    }))
    
    if (!is_valid_multi) {
      stop(paste(
        "Error: The input list is not a valid multi-method output.",
        "This usually happens if you used 'resting_points = TRUE' with HMTS.",
        "Please pass the specific index dataframe to the plot function instead.",
        "Example: plot_price_index(result$Index)"
      ))
    }
    # ==========================================================================
    
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
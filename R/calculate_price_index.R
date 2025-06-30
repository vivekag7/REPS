#' Calculate index based on specified method (Fisher, Laspeyres, Paasche, HMTS, Time Dummy, Rolling Time Dummy)
#'
#' Central hub function to calculate index figures using different methods.
#'
#' @author Vivek Gajadhar
#' @param method One of: "fisher", "laspeyres", "paasche", "hmts", "timedummy", "rolling_timedummy", "repricing"
#' @param dataset Data frame with input data
#' @param period_variable Name of the variable indicating time periods
#' @param dependent_variable Usually the price
#' @param continuous_variables Vector with numeric quality-determining variables
#' @param categorical_variables Vector with categorical variables (also dummies)
#' @param reference_period Period or group of periods that will be set to 100
#' @param diagnostics (HMTS only) Whether to return detailed outputs (default = FALSE)
#' @param periods_in_year (HMTS/Repricing only) Number of periods per year (e.g. 12 for months)
#' @param production_since (HMTS only) Start period for production simulation
#' @param number_preliminary_periods (HMTS only) Number of preliminary periods
#' @param imputation (Laspeyres/Paasche only) Include imputation values? Default = FALSE
#' @param window_length (Rolling Time Dummy only) Window size in number of periods
#'
#' @return A data.frame (or list for HMTS with diagnostics = TRUE; or named list if multiple methods are used)
#' @export
#'
#' @examples
#' # Example: Time Dummy index
#' Tbl_TD <- calculate_price_index(
#'   method = "timedummy",
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   continuous_variables = "floor_area",
#'   categorical_variables = "neighbourhood_code",
#'   reference_period = "2015",
#'   diagnostics = FALSE
#' )
#' head(Tbl_TD)
#'
#' # Example: Multiple methods (Fisher, Paasche, Laspeyres)
#' multi_result <- calculate_price_index(
#'   method = c("fisher", "paasche", "laspeyres"),
#'   dataset = data_constraxion,
#'   period_variable = "period",
#'   dependent_variable = "price",
#'   continuous_variables = "floor_area",
#'   categorical_variables = "neighbourhood_code",
#'   reference_period = "2015",
#'   diagnostics = FALSE
#' )
#'
#' head(multi_result$fisher)
#' head(multi_result$paasche)
#' head(multi_result$laspeyres)


calculate_price_index <- function(method,
                                  dataset,
                                  period_variable,
                                  dependent_variable,
                                  continuous_variables = NULL,
                                  categorical_variables = NULL,
                                  reference_period = NULL,
                                  diagnostics = FALSE,
                                  periods_in_year = 4,
                                  production_since = NULL,
                                  number_preliminary_periods = 3,
                                  imputation = FALSE,
                                  window_length = 5) {
  
  # Prevents call of false methods
  method <- tolower(method)
  valid_methods <- c("fisher", "laspeyres", "paasche", "hmts", "timedummy", "rolling_timedummy", "repricing")
  invalid_methods <- setdiff(method, valid_methods)
  if (length(invalid_methods) > 0) {
    stop(paste0("Invalid method(s): ", paste(invalid_methods, collapse = ", "),
                ". Please choose from: ", paste(valid_methods, collapse = ", "), "."))
  }
  
  # Prevent diagnostics = TRUE in multi-method context
  if (length(method) > 1 && diagnostics) {
    stop("Using 'diagnostics = TRUE' is only allowed with a single method ('hmts').")
  }
  
  validate_input(dataset, period_variable, dependent_variable, continuous_variables, categorical_variables, reference_period)
  
  # Function that runs one method at a time
  run_method <- function(m) {
    if (m == "fisher") {
      return(calculate_fisher(
        dataset = dataset,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        continuous_variables = continuous_variables,
        categorical_variables = categorical_variables,
        reference_period = reference_period,
        diagnostics = diagnostics
      ))
    }
    
    if (m == "laspeyres") {
      return(calculate_laspeyres(
        dataset = dataset,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        continuous_variables = continuous_variables,
        categorical_variables = categorical_variables,
        reference_period = reference_period,
        diagnostics = diagnostics,
        imputation = imputation
      ))
    }
    
    if (m == "paasche") {
      return(calculate_paasche(
        dataset = dataset,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        continuous_variables = continuous_variables,
        categorical_variables = categorical_variables,
        reference_period = reference_period,
        diagnostics = diagnostics,
        imputation = imputation
      ))
    }
    
    if (m == "hmts") {
      return(calculate_hmts(
        dataset = dataset,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        continuous_variables = continuous_variables,
        categorical_variables = categorical_variables,
        reference_period = reference_period,
        periods_in_year = periods_in_year,
        production_since = production_since,
        number_preliminary_periods = number_preliminary_periods,
        diagnostics = diagnostics
      ))
    }
    
    if (m == "timedummy") {
      return(calculate_time_dummy_index(
        dataset = dataset,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        continuous_variables = continuous_variables,
        categorical_variables = categorical_variables,
        reference_period = reference_period,
        diagnostics = diagnostics
      ))
    }
    
    if (m == "rolling_timedummy") {
      if (is.null(window_length)) stop("You must specify 'window_length' for rolling time dummy method.")
      return(calculate_rolling_timedummy_index(
        dataset = dataset,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        continuous_variables = continuous_variables,
        categorical_variables = categorical_variables,
        reference_period = reference_period,
        window_length = window_length,
        diagnostics = diagnostics
      ))
    }
    
    if (m == "repricing") {
      return(calculate_repricing(
        dataset = dataset,
        period_variable = period_variable,
        dependent_variable = dependent_variable,
        continuous_variables = continuous_variables,
        categorical_variables = categorical_variables,
        reference_period = reference_period,
        periods_in_year = periods_in_year,
        diagnostics = diagnostics
      ))
    }
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

#' Plot index output from calculate_price_index (pure plotly, minimal dependencies)
#'
#' Interactive price index plot with a static colorblind-safe palette and clickable legend.
#'
#' @author Vivek Gajadhar
#' @param index_output A data.frame or named list of data.frames
#' @param title Optional plot title
#' @return A plotly object
#' @importFrom plotly plot_ly layout add_trace
#' @importFrom dplyr bind_rows
#' @export
plot_price_index <- function(index_output, title = NULL) {
  get_year_start_periods <- function(periods) {
    years <- substr(periods, 1, 4)
    periods[!duplicated(years)]
  }
  
  # Static colorblind-friendly palette (Okabe-Ito)
  cb_palette <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
  )
  
  is_single <- is.data.frame(index_output)
  
  if (is_single) {
    df <- index_output[order(index_output$period), ]
    breaks <- get_year_start_periods(df$period)
    title_text <- if (is.null(title)) "Price Index" else title
    
    fig <- plotly::plot_ly(
      data = df,
      x = ~period,
      y = ~Index,
      type = 'scatter',
      mode = 'lines+markers',
      name = "Index",
      line = list(color = cb_palette[1]),
      marker = list(color = cb_palette[1])
    )
  } else if (is.list(index_output)) {
    combined <- dplyr::bind_rows(
      lapply(names(index_output), function(name) {
        df <- index_output[[name]]
        df <- df[order(df$period), ]
        df$method <- name
        df
      }),
      .id = NULL
    )
    
    breaks <- get_year_start_periods(combined$period)
    title_text <- if (is.null(title)) "Price Index Comparison" else title
    fig <- plotly::plot_ly()
    
    methods <- unique(combined$method)
    for (i in seq_along(methods)) {
      method_name <- methods[i]
      df <- combined[combined$method == method_name, ]
      color <- cb_palette[(i - 1) %% length(cb_palette) + 1]
      
      fig <- fig %>%
        plotly::add_trace(
          data = df,
          x = ~period,
          y = ~Index,
          type = 'scatter',
          mode = 'lines+markers',
          name = method_name,
          line = list(color = color),
          marker = list(color = color)
        )
    }
  } else {
    stop("Unsupported input type: must be a data.frame or list of data.frames from calculate_price_index()")
  }
  
  fig <- fig %>%
    plotly::layout(
      title = list(text = title_text),
      xaxis = list(
        title = "Period",
        tickvals = breaks,
        tickangle = 45
      ),
      yaxis = list(title = "Index"),
      legend = list(title = list(text = "Method")),
      margin = list(b = 100)
    )
  
  return(fig)
}








### HTMS helper 1

#' Calculate HMTS index only (Hedonic Multilateral Time series re-estimation Splicing)
#'
#' Based on a hedonic model, an index is calculated in below steps. See also Ishaak, Ouwehand, Remoy & De Haan (2023).
#' 1: for each period, average imputed prices are calculated with the first period as base period.
#' 2: step 1 is repeated for every possible base period. This result in as many series of imputed values as the number of periods.
#' 3: All series with imputed prices are re-estimated with a Kalman filter (also time series model/state space model)
#'    This step can be turned off with a parameter.
#' 4: The series of imputed values are transformed into index series.
#' 5: a specified (parameter) window is chosen of index figures that continues in the calculation.
#'    This step can be turned off with a parameter.
#' 6: Of the remaining index figures, the geometric average per period is calculated.
#'    The remaining index figures form the final index.
#'
#' Parameter 'production_since':
#' To simulate a series, where 1 period a time expires (as in production), a manual choice in the past is possible.
#' Until this period, all periods are imputed. After that, 1 period is added.
#'
#' Parameter 'resting_points':
#' If TRUE, the output is a list of tables. These tables can be called with a $ after the output.
#' $Index table with periods, index and number of observations
#' $Window table with the index figures within the chosen window
#' $Chosen_index_series table with index series before the window splice
#' $Matrix_HMTS_index table with index series based on re-estimated imputations (time series model)
#' $Matrix_HMTS table with re-estimated imputations (time series model)
#' $Matrix_HMTS_index table with index series based on estimated imputations (hedonic model)
#' $Matrix_HMTS table with estimated imputations (time series model)l
#' $Matrix_HMTS_analyse table with diagnostic values of the time series model per base period
#'
#' @author Farley Ishaak
#' @param period_variable variable in the dataset with the period
#' @param dependent_variable usually the sale price
#' @param numerical_variables vector with quality-determining continues variables (numeric, no dummies)
#' @param categorical_variables vector with categorical variables (also dummy)
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param production_since 1 period in the format of the period_variable. See description above (default = NULL)
#' @param number_preliminary_periods number of periods that the index is preliminary. Only works if production_since <> NULL. default = 3
#' @param resting_points should analyses values be returned? (default = FALSE)
#' @return
#' $Matrix_HMTS_index table with index series based on estimations with time series re-estimations
#' $Matrix_HMTS table with estimated values based on time series re-estimations
#' $Matrix_HMS_index table with index series based on estimations with the hedonic model
#' $Matrix_HMS table with estimated values based on the hedonic model
#' $Matrix_HMTS_analysis table with analysis values of the time series model per base period
#' @keywords internal
#' @noRd
#' @return table with periods, index and number of observations. If resting_points = TRUE, then list with tables. See general description and examples.
#' @importFrom dplyr bind_rows select all_of 

calculate_hmts_index <- function(
    dataset,
    period_variable,
    dependent_variable,
    numerical_variables,
    categorical_variables,
    reference_period,
    production_since = NULL,
    number_preliminary_periods,
    number_of_observations = NULL,
    resting_points) {
  
  period_list <- sort(unique(dataset$period))
  number_of_periods <- length(period_list)
  
  if (is.null(number_preliminary_periods) == TRUE) {
    number_preliminary_periods <- number_of_periods
  }
  
  imputations_complete <- calculate_hedonic_imputationmatrix(dataset = dataset
                                                             , period_variable = "period"
                                                             , dependent_variable = dependent_variable
                                                             , numerical_variables = numerical_variables
                                                             , categorical_variables = categorical_variables
                                                             , number_of_observations = number_of_observations
                                                             , production_since = production_since
                                                             , number_preliminary_periods = number_preliminary_periods)
  
  matrix_hmts <- as.data.frame(imputations_complete$matrix_hmts)
  matrix_hmts_index <- as.data.frame(imputations_complete$matrix_hmts_index)
  matrix_hmts_analysis <- as.data.frame(imputations_complete$matrix_hmts_analysis)
  imputations <- matrix_hmts_index
  
  start_window <- 2
  
  if (number_preliminary_periods != number_of_periods) {
    window_rows <- vector("list", number_of_periods)
    
    for (current_period in 1:number_of_periods) {
      
      if (current_period == 1) {
        window_row <- imputations[current_period, c(current_period:(number_preliminary_periods + start_window))]
        window_row$period <- NULL
        end_window <- start_window
        start_window_update <- start_window + 1
      }
      
      if (current_period > 1 && current_period <= number_preliminary_periods + 1) {
        # end_window <- end_window + 1
        end_window <- number_preliminary_periods + start_window
        window_row <- as.data.frame(imputations[current_period, c(start_window:end_window)])
      }
      
      if (current_period > number_preliminary_periods + 1) {
        end_window <- end_window + 1
        if (number_preliminary_periods == 0) {
          window_row <- as.data.frame(imputations[current_period, c((start_window_update - 1):end_window)])
          window_row[, 1] <- NA
        } else {
          window_row <- as.data.frame(imputations[current_period, c(start_window_update:end_window)])
        }
        start_window_update <- start_window_update + 1
      }
      
      window_rows[[current_period]] <- window_row
    }
    
    window <- dplyr::bind_rows(window_rows)
  }
  
  if (number_preliminary_periods == number_of_periods) {
    window <- imputations[, -1]
  }
  
  window_transposed <- t(window)
  geometric_averages <- vapply(seq_len(number_of_periods), function(current_period) {
    calculate_geometric_average(stats::na.omit(window_transposed[, current_period]))
  }, numeric(1))
  
  window$period <- period_list
  window <- window[, c(number_of_periods + 1, 1:number_of_periods)]
  matrix_hmts$geom_avg <- matrix_hmts_index$geom_avg <- window$geom_avg <- geometric_averages
  imputations$Index <- matrix_hmts$index <- matrix_hmts_index$index <- window$index <- calculate_index(period_list, geometric_averages, reference_period = reference_period)
  
  if (number_of_observations == TRUE) {
    results <- dplyr::select(imputations, dplyr::all_of(c("period", "number_of_observations", "Index")))
  } else {
    results <- dplyr::select(imputations, dplyr::all_of(c("period", "Index")))
  }
  
  if (resting_points == TRUE) {
    results <- list(Index = results
                    , window = window
                    , chosen_index_series = imputations
                    , matrix_hmts_index = matrix_hmts_index
                    , matrix_hmts = matrix_hmts
                    , matrix_hmts_analysis = matrix_hmts_analysis)
  }
  
  return(results)
}


### HMTS helper 2

#' Calculate a matrix with hedonic imputation averages, re-estimated time series imputation averages and  corresponding index series.
#'
#' Based on a hedonic model, a series of imputed values is calculated in below steps:
#' 1: for every period average imputed prices are estimated with the 1st period as base period.
#' 2: the above is repeated for each possible base period. This result in an equal number of series as the number of periods.
#' 3: All series are re-estimated with a time series model (state space).
#'    This step is optionally skipped with a parameter (state_space_model = NULL)
#' 4: the series imputed values are transformed into index series.
#' This matrix can be used for an index calculations according to the HMTS method.
#'
#' Parameter 'production_since':
#' To simulate a series, where 1 period a time expires (as in production), a manual choice in the past is possible.
#' Until this period, all periods are imputed. After that, 1 period is added.
#'
#' @author Farley Ishaak
#' @param dataset table with data (does not need to be a selection of relevant variables)
#' @param period_variable variable in the dataset with the period
#' @param dependent_variable usually the sale price
#' @param numerical_variables vector with quality-determining continues variables (numeric, no dummies)
#' @param categorical_variables vector with categorical variables (also dummy)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param production_since 1 period in the format of the period_variable. See description above (default = NULL)
#' @param number_preliminary_periods number of periods that the index is preliminary. Only works if production_since <> NULL. default = 3
#' @return
#' $Matrix_HMTS_index table with index series based on estimations with time series re-estimations
#' $Matrix_HMTS table with estimated values based on time series re-estimations
#' $Matrix_HMS_index table with index series based on estimations with the hedonic model
#' $Matrix_HMS table with estimated values based on the hedonic model
#' $Matrix_HMTS_analysis table with analysis values of the time series model per base period
#' @keywords internal
#' @noRd
#' @importFrom stats lm.fit model.frame model.matrix model.response


calculate_hedonic_imputationmatrix <- function(dataset
                                               , period_variable
                                               , dependent_variable
                                               , numerical_variables
                                               , categorical_variables
                                               , number_of_observations = TRUE
                                               , production_since = NULL
                                               , number_preliminary_periods) {
  
  # Merge all hedonic variables
  independent_variables <- c(numerical_variables, categorical_variables)
  
  # Make and sort list of periods
  period_list <- sort(unique(as.character(dataset$period)))
  number_periods <- length(period_list)
  
  if (!is.null(production_since)) {
    production_since_index <- match(production_since, period_list)
    if (is.na(production_since_index)) {
      stop("The provided production_since-period is not part of the periods in the data. Check the notation.")
    }
  } else {
    production_since_index <- number_preliminary_periods <- number_periods
  }
  
  dataset_temp <- dataset[, (names(dataset) %in% c("period", dependent_variable, independent_variables))]
  
  text_columns <- vapply(dataset_temp, function(column) {
    is.character(column) || is.factor(column)
  }, logical(1))
  for (column_name in names(dataset_temp)[text_columns]) {
    dataset_temp[[column_name]][dataset_temp[[column_name]] == ""] <- NA
  }
  dataset_temp <- dataset_temp[stats::complete.cases(dataset_temp), , drop = FALSE]
  
  dataset_temp <- droplevels(dataset_temp)
  
  
  dependent_variable <- paste0("log(", dependent_variable, ")")
  
  # Determine linear regression model
  model <- paste(dependent_variable, "~", paste(independent_variables, collapse=" + "))

  number_observations_total <- if (number_of_observations == TRUE) integer(number_periods) else NULL
  base_column_names <- paste0("Base_", period_list)
  matrix_hmts_values <- matrix(
    NA_real_,
    nrow = number_periods,
    ncol = number_periods,
    dimnames = list(NULL, base_column_names)
  )
  matrix_hmts_index_values <- matrix_hmts_values
  
  # Determine all levels for all factor variables on the complete data set
  for (cat_var in categorical_variables) {
    if (is.character(dataset_temp[[cat_var]]) || is.factor(dataset_temp[[cat_var]])) {
      dataset_temp[[cat_var]] <- factor(dataset_temp[[cat_var]], levels = unique(dataset_temp[[cat_var]]))
    }
  }
  
  # Sort variables in data according to the model
  mf_all <- model.frame(model, as.data.frame(dataset_temp))
  X_all  <- model.matrix(as.formula(model), mf_all) # hedonic variables (x)
  y_all  <- model.response(mf_all) # prices (y)
  
  # Determine per period the corresponding row
  rows_by_period <- split(seq_len(nrow(dataset_temp)), dataset_temp[[period_variable]])
  coefficients_by_period <- vector("list", number_periods)

  for (reporting_period in seq_len(number_periods)) {
    rows_dynamic <- rows_by_period[[period_list[reporting_period]]]
    X_dyn <- X_all[rows_dynamic, , drop = FALSE]
    y_dyn <- y_all[rows_dynamic]

    fitcoef <- lm.fit(X_dyn, y_dyn)$coefficients
    fitcoef[is.na(fitcoef)] <- 0
    coefficients_by_period[[reporting_period]] <- fitcoef
  }
  
  # Loop through all possible base periods
  for (current_period in seq_len(number_periods)) {
    
    # Determine the difference between the number of periods and number of periods from the moment of production
    number_periods_production_since <- if (current_period <= production_since_index - number_preliminary_periods) {
      production_since_index
    } else {
      current_period + number_preliminary_periods
    }
    number_periods_production_since <- min(number_periods_production_since, number_periods)
    
    # Filter (and index) row numbers per base period
    rows_base <- rows_by_period[[ period_list[current_period] ]]
    X_base <- X_all[rows_base, , drop = FALSE] # hedonic variables (for 1 period)
    X_base_means <- colMeans(X_base)
    
    # Count all rows for indicator 'number of transactions'
    if (number_of_observations == TRUE) {
      number_observations_total[current_period] <- nrow(X_base)
    }
    
    # Define vector for all matrix calculations
    hms <- numeric(number_periods_production_since)

    # Loop through all possible reporting periods (within the same base period)
    for (reporting_period in seq_len(number_periods_production_since)) {
      fitcoef <- coefficients_by_period[[reporting_period]]
      hms[reporting_period] <- exp(sum(X_base_means * fitcoef)) # Transform log values back
      
    }
    
    
    hmts_temp <- calculate_trend_line_kfas(original_series = hms, resting_points = TRUE)
    hmts <- hmts_temp$trend_line
    hmts_analysis <- hmts_temp$resting_points
    hmts_index <- calculate_index(periods = c(1:number_periods_production_since), values = hmts)
    
    calculated_rows <- seq_len(number_periods_production_since)
    matrix_hmts_values[calculated_rows, current_period] <- hmts
    matrix_hmts_index_values[calculated_rows, current_period] <- hmts_index
    
    if (current_period == 1) {
      matrix_hmts_analysis <- data.frame(reeks = hmts_analysis$reeks) 
    }
    matrix_hmts_analysis[paste0("Base_", period_list[current_period])] <- hmts_analysis$value
  }
  
  matrix_hmts <- data.frame(period = period_list, matrix_hmts_values, check.names = FALSE)
  matrix_hmts_index <- data.frame(period = period_list, matrix_hmts_index_values, check.names = FALSE)

  # Add numbers to calculation
  if (number_of_observations == TRUE) {
    matrix_hmts["number_of_observations"] <- matrix_hmts_index["number_of_observations"] <- number_observations_total
  }
  
  matrices <- list(matrix_hmts = matrix_hmts
                   , matrix_hmts_index = matrix_hmts_index
                   , matrix_hmts_analysis = matrix_hmts_analysis)
  
  return(matrices)
  
}

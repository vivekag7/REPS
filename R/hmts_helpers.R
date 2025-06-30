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
#' Parameter 'diagnostics':
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
#' @param continuous_variables vector with quality-determining continues variables (numeric, no dummies)
#' @param categorical_variables vector with categorical variables (also dummy)
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param diagnostics number of observations per period (default = TRUE)
#' @param periods_in_year if month, then 12. If quarter, then 4, etc. (default = 4)
#' @param production_since 1 period in the format of the period_variable. See description above (default = NULL)
#' @param number_preliminary_periods number of periods that the index is preliminary. Only works if production_since <> NULL. default = 3
#' @param diagnostics should analyses values be returned? (default = FALSE)
#' @return
#' $Matrix_HMTS_index table with index series based on estimations with time series re-estimations
#' $Matrix_HMTS table with estimated values based on time series re-estimations
#' $Matrix_HMS_index table with index series based on estimations with the hedonic model
#' $Matrix_HMS table with estimated values based on the hedonic model
#' $Matrix_HMTS_analysis table with analysis values of the time series model per base period
#' @keywords internal
#' @return table with periods, index and number of observations. If diagnostics = TRUE, then list with tables. See general description and examples.


calculate_hmts_index <- function(
    dataset,
    period_variable,
    dependent_variable,
    continuous_variables,
    categorical_variables,
    reference_period,
    periods_in_year,
    production_since = NULL,
    number_preliminary_periods,
    diagnostics) {
  
  period_list <- sort(unique(dataset$period))
  number_of_periods <- length(period_list)
  
  if (is.null(number_preliminary_periods) == TRUE) {
    number_preliminary_periods <- number_of_periods
  }
  
  imputations_complete <- calculate_hedonic_imputationmatrix(dataset = dataset
                                                             , period_variable = "period"
                                                             , dependent_variable = dependent_variable
                                                             , continuous_variables = continuous_variables
                                                             , categorical_variables = categorical_variables
                                                             , periods_in_year = periods_in_year
                                                             , diagnostics = diagnostics
                                                             , production_since = production_since
                                                             , number_preliminary_periods = number_preliminary_periods)
  
  matrix_hmts <- as.data.frame(imputations_complete$matrix_hmts)
  matrix_hmts_index <- as.data.frame(imputations_complete$matrix_hmts_index)
  matrix_hmts_analysis <- as.data.frame(imputations_complete$matrix_hmts_analysis)
  imputations <- matrix_hmts_index
  
  start_window <- 2
  
  if (number_preliminary_periods != number_of_periods) {
    
    for (current_period in 1:number_of_periods) {
      
      if (current_period == 1) {
        window <- imputations[current_period, c(current_period:(number_preliminary_periods + start_window))]
        window$period <- NULL
        end_window <- start_window
        start_window_update <- start_window + 1
      }
      
      if (current_period > 1 && current_period <= number_preliminary_periods + 1) {
        # end_window <- end_window + 1
        end_window <- number_preliminary_periods + start_window
        window <- dplyr::bind_rows(window, as.data.frame(imputations[current_period, c(start_window:end_window)]))
      }
      
      if (current_period > number_preliminary_periods + 1) {
        end_window <- end_window + 1
        if (number_preliminary_periods == 0) {
          window_plus_1 <- as.data.frame(imputations[current_period, c((start_window_update - 1):end_window)])
          window_plus_1[, 1] <- NA
          window <- dplyr::bind_rows(window, window_plus_1)
        } else {
          window <- dplyr::bind_rows(window, as.data.frame(imputations[current_period, c(start_window_update:end_window)]))
        }
        start_window_update <- start_window_update + 1
      }
      
    }
    
  }
  
  if (number_preliminary_periods == number_of_periods) {
    window <- imputations[, -1]
  }
  
  window_transposed <- as.data.frame(t(window))
  geometric_averages <- c()
  
  for (current_period in 1:number_of_periods) {
    
    geometric_average <- calculate_geometric_average(na.omit(window_transposed[, current_period]))
    
    geometric_averages[current_period] <- geometric_average
    
  }
  
  window$period <- period_list
  window <- window[, c(number_of_periods + 1, 1:number_of_periods)]
  matrix_hmts$geom_avg <- matrix_hmts_index$geom_avg <- window$geom_avg <- geometric_averages
  imputations$Index <- matrix_hmts$index <- matrix_hmts_index$index <- window$index <- calculate_index(period_list, geometric_averages, reference_period = reference_period)
  
  if (diagnostics == TRUE) {
    results <- dplyr::select(imputations, dplyr::all_of(c("period", "number_of_observations", "Index")))
  } else {
    results <- dplyr::select(imputations, dplyr::all_of(c("period", "Index")))
  }
  
  if (diagnostics == TRUE) {
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
#' @param continuous_variables vector with quality-determining continues variables (numeric, no dummies)
#' @param categorical_variables vector with categorical variables (also dummy)
#' @param diagnostics number of observations per period (default = TRUE)
#' @param periods_in_year if month, then 12. If quarter, then 4, etc. (default = 4)
#' @param production_since 1 period in the format of the period_variable. See description above (default = NULL)
#' @param number_preliminary_periods number of periods that the index is preliminary. Only works if production_since <> NULL. default = 3
#' @return
#' $Matrix_HMTS_index table with index series based on estimations with time series re-estimations
#' $Matrix_HMTS table with estimated values based on time series re-estimations
#' $Matrix_HMS_index table with index series based on estimations with the hedonic model
#' $Matrix_HMS table with estimated values based on the hedonic model
#' $Matrix_HMTS_analysis table with analysis values of the time series model per base period
#' @keywords internal

calculate_hedonic_imputationmatrix <- function(dataset
                                               , period_variable
                                               , dependent_variable
                                               , continuous_variables
                                               , categorical_variables
                                               , periods_in_year
                                               , diagnostics = TRUE
                                               , production_since = NULL
                                               , number_preliminary_periods) {
  
  independent_variables <- c(continuous_variables, categorical_variables)
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
  
  dataset_temp[dataset_temp == ""] <- NA
  dataset_temp <- stats::na.omit(dataset_temp)
  
  dataset_temp <- droplevels(dataset_temp)
  
  
  dependent_variable <- paste0("log(", dependent_variable, ")")
  
  
  for (indep_var in 1:length(independent_variables)) {
    if (indep_var == 1) {
      model <- paste0(dependent_variable, "~", independent_variables[indep_var])
    } else {
      model <- paste0(model, "+", independent_variables[indep_var])
    }
  }
  
  number_observations_total <- c()
  
  matrix_hmts <- matrix_hmts_index <- data.frame(period=period_list)
  
  for (current_period in 1:number_periods) {
    
    if (current_period <= production_since_index - number_preliminary_periods) {
      number_periods_production_since <- production_since_index
    }
    if (current_period > production_since_index - number_preliminary_periods) {
      number_periods_production_since <- current_period + number_preliminary_periods
    }
    if (number_periods_production_since > number_periods) {
      number_periods_production_since <- number_periods
    }
    
    difference_length_series <- number_periods - number_periods_production_since
    
    dataset_base <- subset(dataset_temp, dataset_temp[[period_variable]] == period_list[current_period])
    
    if (diagnostics == TRUE) {
      number_observations_total[current_period] <- nrow(dataset_base)
    }
    
    hms <- c()
    hmts <- c()
    hmts_index <- c()
    hmts_analysis <- c()
    
    for (reporting_period in 1:number_periods_production_since) {
      dataset_dynamic <- subset(dataset_temp, dataset_temp[[period_variable]] == period_list[reporting_period])
      fitmdl <- stats::lm(model, dataset_dynamic)
      
      for (var in names(fitmdl$xlevels)) {
        missend_in_model <- levels(dataset_base[[var]])[!(levels(dataset_base[[var]]) %in% fitmdl$xlevels[[var]])]
        sel <- dataset_base[[var]] %in% missend_in_model
        dataset_base[[var]][sel] <- fitmdl$xlevels[[var]][1]
      }
      
      predictmdl <- mean(stats::predict(fitmdl, dataset_base))
      predictmdl <- exp(predictmdl)
      hms[reporting_period] <- predictmdl
      
    }
    
    
    hmts_temp <- calculate_trend_line_kfas(original_series = hms, periodicity = periods_in_year, diagnostics = TRUE)
    hmts <- hmts_temp$trend_line
    hmts_analysis <- hmts_temp$diagnostics
    hmts_index <- calculate_index(periods = c(1:number_periods_production_since), values = hmts)
    
    
    hmts <- c(hmts, rep(NA, difference_length_series))
    hmts_index <- c(hmts_index, rep(NA, difference_length_series))
    
    matrix_hmts[paste0("Base_", period_list[current_period])] <- hmts
    matrix_hmts_index[paste0("Base_", period_list[current_period])] <- hmts_index
    
    if (current_period == 1) {
      matrix_hmts_analysis <- data.frame(reeks = hmts_analysis$reeks) 
    }
    matrix_hmts_analysis[paste0("Base_", period_list[current_period])] <- hmts_analysis$value
  }
  
  # Add numbers to calculation
  if (diagnostics == TRUE) {
    matrix_hmts["number_of_observations"] <- matrix_hmts_index["number_of_observations"] <- number_observations_total
  }
  
  matrices <- list(matrix_hmts = matrix_hmts
                   , matrix_hmts_index = matrix_hmts_index
                   , matrix_hmts_analysis = matrix_hmts_analysis)
  
  return(matrices)
  
}

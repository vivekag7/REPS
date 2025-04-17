### This is the first internal function

#' Calculate imputation averages with the 1st period as base period
#'
#' Prices are estimated based on a provided Hedonic model
#' The model values are calculated for each period in the data
#' With these values, new prices of base period observations are estimated.
#' With this function, imputations according to the Laspeyres and Paasche method can be estimated.
#'
#' @author Farley Ishaak
#' @param dataset_temp table with data 
#' @param period_temp 'period'
#' @param dependent_variable_temp usually the sale price
#' @param independent_variables_temp vector with quality determining variables
#' @param period_list_temp list with all available periods
#' @return
#' Table with imputation averages per period
#' @keywords internal


calculate_hedonic_imputation <- function(dataset_temp = dataset
                                         , period_temp = "period_var_temp"
                                         , dependent_variable_temp = dependent_variable
                                         , independent_variables_temp = independent_variables
                                         , number_of_observations_temp = number_of_observations
                                         , period_list_temp = period_list) {
  
  # Count number of periods
  number_of_periods <- length(period_list_temp)
  
  # Select required variables
  dataset_temp <- dataset_temp |>
    dplyr::select(all_of(c(period_temp, dependent_variable_temp,
                           independent_variables_temp)))
  
  # Remove lines without values
  dataset_temp[dataset_temp == ''] <- NA
  dataset_temp <- stats::na.omit(dataset_temp)
  
  # Remove unused levels. R remembers the original state of the levels, but if a level is not present in a certain period, this may result in an error in the bootstrap.
  dataset_temp <- droplevels(dataset_temp)
  
  
  dependent_variable_temp <- paste0("log(", dependent_variable_temp, ")")
  
  
  ## Model
  
  # Construct the regression formula from independent variables.
  for (indep_var in 1: length(independent_variables_temp)) {
    if (indep_var == 1) {
      model <- paste0(dependent_variable_temp, "~", independent_variables_temp[indep_var])
    } else {
      model <- paste0(model, "+", independent_variables_temp[indep_var])
    }
  }
  
  ## Calculate imputations per period
  
  # Empty vector for the values and numbers
  average_imputations <- c()
  number_observations_total <- c()
  
  for (current_period in 1:number_of_periods) {
    
    # Estimate coefficients of the 1st period
    if (current_period == 1) {
      rekenbestand <- subset(dataset_temp, period_var_temp == period_list_temp[1])
      fitmdl <- stats::lm(model, rekenbestand)
      predictmdl_0 <- mean(stats::predict(fitmdl, rekenbestand))
      

      predictmdl_0 <- exp(predictmdl_0)

      if (number_of_observations_temp == TRUE) {
        number <- nrow(rekenbestand)
      }
    } else {
      # Estimate coefficients of all periods after
      rekenbestand_t <- subset(dataset_temp, period_var_temp == period_list_temp[current_period])
      fitmdl <- stats::lm(model, rekenbestand_t)
      
      # If parameter number_of_observations = TRUE, then calculate numbers
      if (number_of_observations_temp == TRUE) {
        number <- nrow(rekenbestand_t)
      }
    }
    
    # Recoding of values, where the categorical variable has a level that is not estimated in the reference period
    rekenbestand_0 <- rekenbestand
    for (var in names(fitmdl$xlevels)) {
      missend_in_model <- levels(rekenbestand_0[[var]])[!(levels(rekenbestand_0[[var]]) %in% fitmdl$xlevels[[var]])]
      
      # Replace level of the variable by the first level (default). The variable is in fact not taken into account by this step in the calculation.
      sel <- rekenbestand_0[[var]] %in% missend_in_model
      rekenbestand_0[[var]][sel] <- fitmdl$xlevels[[var]][1]
    }
    predictmdl_t <- mean(predict(fitmdl, rekenbestand_0))
    
    predictmdl_t <- exp(predictmdl_t)
    
    average_imputations[current_period] <- predictmdl_t
    
    # If parameter number_of_observations = TRUE, then add numbers to table
    if (number_of_observations_temp == TRUE) {
      number_observations_total[current_period] <- number
    }
  }
  
  # Create table
  tbl_average_imputation <- data.frame(period = period_list_temp)
  
  # If parameter number_of_observations = TRUE, then add numbers to table
  if (number_of_observations_temp == TRUE) {
    tbl_average_imputation$number_of_observations <- number_observations_total
  }
  
  # Add imputations to table
  tbl_average_imputation$average_imputation <- average_imputations
  
  # Result
  return(tbl_average_imputation)
  
}

### This is the second internal function


#' Transform series into index
#'
#' The index can be calculated in two ways:
#' - from a series of values
#' - from a series of mutations (from_growth_rate = TRUE)
#'
#' N.B. with from_growth_rate:
#' The series of mutations must be equally long to the series of values.
#' The vector should, therefore, also contain a mutation for the first period (this is likely 1).
#' In the calculation, this first mutation is not used.
#'
#' N.B. for the reference period:
#' The first value is on default set to 100.
#' An adjusted reference period can be provided in the paramater.
#' The reference period can also be a part of a period.
#' E.g. if the series contains months (2019jan, 2019feb), the reference period can be a year (2019).
#'
#' @author Farley Ishaak
#' @param periods vector/variable with periods (numeric/string)
#' @param values vector/variable with to be transformed values (numeric)
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @return Index series
#' @keywords internal

calculate_index <- function(periods
                            , values
                            , reference_period = NULL) {
  
  # Check length periods and values
  if (length(periods) != length(values)) {
    stop("The periods variable is not of the same length as the values variable.")
  }
  
  # Check numeric values
  if (is.numeric(values) == FALSE) {
    stop("The values variable is not (fully) numeric.")
  }
  
  # Transforms periods to characters
  periods <- as.character(periods)
  
  # If reference_period is not provided, then reference_period = 1st period from list
  if (is.null(reference_period) == TRUE) {
    reference_period <- periods[1]
    periods_short <- periods
  } else {
    # Determine length reference_period
    length_reference_period <- nchar(reference_period)
    periods_short <- substr(periods, 1, length_reference_period)
  }
  
  # Check reference_period
  if (!(reference_period %in% periods_short)) {
    stop("The provided reference period is not part of the series with periods")
  }
  
  
  # Create table
  tbl_index <- data.frame(period = periods_short, value = values)
  
  average <- tbl_index |>
    dplyr::filter(period == reference_period) |>
    dplyr::summarise(value = mean(value)) |>
    dplyr::pull(value)
  
  # Calculate index
  tbl_index$Index <- tbl_index$value / average * 100
  
  # Result = index series
  return(index = tbl_index$Index)
  
}


### This is the third internal function

#' Function for showing progress loop
#'
#' @param single_iteration a single iteration (usually 1 letter: i or p)
#' @param total_iterations the total number of iterations in the loop
#'
#' @return this returns a progress text
#' @keywords internal
show_progress_loop <- function(single_iteration
                               , total_iterations){
  
  # Print dynamic progress indicator in the console
  cat(sprintf("\rProgress: %3d%%", round(single_iteration / total_iterations * 100)))
  if (single_iteration == total_iterations) message("\n Done!")
  
}

### This is the fourth internal function
#' Validate Input Data for Hedonic Index Calculation
#'
#' This function checks whether the dataset contains all required variables, whether the dependent and continuous variables are numeric, 
#' and whether the period variable is formatted correctly (e.g., "2020Q1", "2020M01").
#' It ensures that the data is suitable for further processing in hedonic index calculations.
#'
#' @param dataset A data.frame containing the dataset to be validated.
#' @param period_variable A string specifying the name of the period variable column.
#' @param dependent_variable A string specifying the name of the dependent variable (usually the sale price).
#' @param continuous_variables A character vector with names of numeric quality-determining variables.
#' @param categorical_variables A character vector with names of categorical variables (including dummies).
#'
#' @return Returns TRUE invisibly if all checks pass. Otherwise, an error is thrown.
#'
#' @author David Pietersz
#' @keywords internal
#' 
#' @importFrom assertthat assert_that has_name
#' @importFrom stringr str_detect

validate_input <- function(dataset, period_variable, dependent_variable, continuous_variables, categorical_variables) {
  
  # Dataset contains all necessary columns
  assertthat::assert_that(assertthat::has_name(dataset, c(period_variable, dependent_variable, continuous_variables, categorical_variables)))
  
  # Dependent and continuous variables only contain numeric values
  numeric_cols <- c(dependent_variable, continuous_variables)
  for (col in numeric_cols) {
    assertthat::assert_that(is.numeric(dataset[[col]]), msg = paste("Column", col, "is not (fully) numeric."))
  }
  
  
  # If log transformation still needs to be performed, dependent variable should contain strictly positive values.
  assertthat::assert_that(all(dataset[[dependent_variable]] > 0), msg = "The dependent variable contains negative values while log transformation needs to be performed.")
 
  
  regex_period <- "^[0-9]{4}([Mm](0?[1-9]|1[0-2])|[Qq](0?[1-4]))$"
  
  assertthat::assert_that(all(stringr::str_detect(dataset[[period_variable]], regex_period)), msg = "The period variable should be in the correct format, for example: 2020Q1, 2020q1, 2020M1, 2020M01, 2020m01, 2020Q4, 2020q04.")
  
  
}

## HTMS helper 1

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
#' @param continuous_variables vector with quality-determining continues variables (numeric, no dummies)
#' @param categorical_variables vector with categorical variables (also dummy)
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param periods_in_year if month, then 12. If quarter, then 4, etc. (default = 4)
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
#' @return table with periods, index and number of observations. If resting_points = TRUE, then list with tables. See general description and examples.


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
                                                             , continuous_variables = continuous_variables
                                                             , categorical_variables = categorical_variables
                                                             , periods_in_year = periods_in_year
                                                             , number_of_observations = number_of_observations
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
  imputations$index <- matrix_hmts$index <- matrix_hmts_index$index <- window$index <- calculate_index(period_list, geometric_averages, reference_period = reference_period)
  
  if (number_of_observations == TRUE) {
    results <- dplyr::select(imputations, dplyr::all_of(c("period", "index", "number_observations")))
  } else {
    results <- dplyr::select(imputations, dplyr::all_of(c("period", "index")))
  }
  
  if (resting_points == TRUE) {
    results <- list(index = results
                    , window = window
                    , chosen_index_series = imputations
                    , matrix_hmts_index = matrix_hmts_index
                    , matrix_hmts = matrix_hmts
                    , matrix_hmts_analysis = matrix_hmts_analysis)
  }
  
  return(results)
}


## HMTS helper 2

#' Calculate the geometric average of a series of values
#'
#' The equation for the calculation is:: exp(mean(log(series_values)))
#'
#' @author Farley Ishaak 
#' @param values series with numeric values
#' @return geometric average
#' @keywords Internal

calculate_geometric_average <- function(values){
  
  # Remove NA values
  values <- values[!is.na(values)]
  
  assertthat::assert_that(is.numeric(values), msg = "Values is not (fully) numeric")
  
  return(exp(mean(log(values))))
  
}

## HMTS helper 3

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
#' @param number_of_observations number of observations per period (default = TRUE)
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
                                               , number_of_observations = TRUE
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
    
    if (number_of_observations == TRUE) {
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
    
    
    hmts_temp <- calculate_trend_line_kfas(original_series = hms, periodicity = periods_in_year, resting_points = TRUE)
    hmts <- hmts_temp$trend_line
    hmts_analysis <- hmts_temp$resting_points
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
  if (number_of_observations == TRUE) {
    matrix_hmts["number_observations"] <- matrix_hmts_index["number_observations"] <- number_observations_total
  }
  
  matrices <- list(matrix_hmts = matrix_hmts
                   , matrix_hmts_index = matrix_hmts_index
                   , matrix_hmts_analysis = matrix_hmts_analysis)
  
  return(matrices)
  
}







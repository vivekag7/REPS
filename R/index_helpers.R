### Helper 1

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


calculate_hedonic_imputation <- function(dataset_temp
                                         , period_temp
                                         , dependent_variable_temp 
                                         , independent_variables_temp 
                                         , number_of_observations_temp 
                                         , period_list_temp ) {
  
  # Count number of periods
  number_of_periods <- length(period_list_temp)
  
  # Select required variables
  dataset_temp <- dataset_temp[, c(period_temp, dependent_variable_temp, independent_variables_temp), drop = FALSE]
  
  
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
      rekenbestand <- dataset_temp[dataset_temp[[period_temp]] == period_list_temp[1], , drop = FALSE]
      fitmdl <- stats::lm(model, rekenbestand)
      predictmdl_0 <- mean(stats::predict(fitmdl, rekenbestand))
      predictmdl_0 <- exp(predictmdl_0)
      
      if (number_of_observations_temp == TRUE) {
        number <- nrow(rekenbestand)
      }
    } else {
      # Estimate coefficients of all periods after
      rekenbestand_t <- dataset_temp[dataset_temp[[period_temp]] == period_list_temp[current_period], , drop = FALSE]
      fitmdl <- stats::lm(model, rekenbestand_t)
      
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

### Helper 2


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
  average <- mean(tbl_index[tbl_index[["period"]] == reference_period, "value"], na.rm = TRUE)

  # Calculate index
  tbl_index$Index <- tbl_index$value / average * 100
  
  # Result = index series
  return(index = tbl_index$Index)
  
}

### Helper 3
#' Calculate Growth Rates
#'
#' Computes period-over-period growth rates from a numeric index vector.
#'
#' @param values A numeric vector representing index values.
#' @return A numeric vector of growth rates, with 1 as the initial value.
#' @author Vivek Gajadhar
#' @keywords internal
#' @importFrom utils head

calculate_growth_rate <- function(values) {
  if (!is.numeric(values)) stop("The series of values is not fully numeric.")
  values <- as.numeric(values)
  growth_rate <- values / c(NA, head(values, -1))
  growth_rate[1] <- 1
  return(growth_rate)
}



### Helper 4

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
  
  return(exp(mean(log(values))))
  
}






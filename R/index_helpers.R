#' Prepare Data for Hedonic Modeling
#'
#' Standardizes the dataset by coercing categorical variables to factors,
#' ensuring the period variable is properly formatted, and optionally 
#' log-transforming the dependent variable. Also handles NA removal consistently.
#'
#' @author Vivek Gajadhar
#' @param dataset A data.frame.
#' @param period_variable String name of the period column.
#' @param dependent_variable String name of the dependent variable.
#' @param numerical_variables Character vector of numerical variables.
#' @param categorical_variables Character vector of categorical variables.
#' @param log_dependent Logical, whether to log-transform the dependent variable (creates a new column with the prefix "log_").
#' @return A cleaned, formatted data.frame.
#' @keywords internal
#' @noRd
prepare_hedonic_data <- function(dataset, period_variable, dependent_variable, numerical_variables, categorical_variables, log_dependent = FALSE) {
  
  # Keep only required columns to avoid dropping rows due to NAs in unused columns
  vars_to_keep <- c(period_variable, dependent_variable, numerical_variables, categorical_variables)
  dataset <- dataset[, colnames(dataset) %in% vars_to_keep, drop = FALSE]
  
  # Standard NA and empty string removal
  text_columns <- vapply(dataset, function(column) {
    is.character(column) || is.factor(column)
  }, logical(1))
  for (column_name in names(dataset)[text_columns]) {
    dataset[[column_name]][dataset[[column_name]] == ""] <- NA
  }
  dataset <- dataset[stats::complete.cases(dataset), , drop = FALSE]
  dataset <- droplevels(dataset)
  
  # Standardize variable types
  dataset[[period_variable]] <- as.character(dataset[[period_variable]])
  for (var in categorical_variables) {
    dataset[[var]] <- as.factor(dataset[[var]])
  }
  
  # Safe log transformation
  if (log_dependent) {
    if (any(dataset[[dependent_variable]] <= 0, na.rm = TRUE)) {
      stop("Dependent variable contains non-positive values; cannot apply log transformation.")
    }
    dataset[[paste0("log_", dependent_variable)]] <- log(dataset[[dependent_variable]])
  }
  
  return(dataset)
}

#' Format Hedonic Index Output
#'
#' Constructs the standardized output table, handling observation counts,
#' rebasing to the reference period, and enforcing strict column order.
#'
#' @author Vivek Gajadhar
#' @param periods Vector of period identifiers.
#' @param index_values Numeric vector of calculated index values.
#' @param reference_period String indicating the base period (can be NULL).
#' @param observation_counts Integer vector of observation counts (optional).
#' @return A standardized data.frame with output columns  period, number_of_observations and Index
#' @keywords internal
#' @noRd
format_index_output <- function(periods, index_values, reference_period = NULL, observation_counts = NULL) {
  
  # Build base table
  res <- data.frame(
    period = as.character(periods),
    Index = as.numeric(index_values)
  )
  
  # Apply rebasing if requested
  if (!is.null(reference_period)) {
    res$Index <- calculate_index(res$period, res$Index, reference_period)
  }
  
  # Attach observation counts and enforce exact column order
  if (!is.null(observation_counts)) {
    res$number_of_observations <- as.integer(observation_counts)
    res <- res[, c("period", "number_of_observations", "Index")]
  } else {
    res <- res[, c("period", "Index")]
  }
  
  return(res)
}

#' Fit a Hedonic Linear Model
#'
#' A centralized helper to construct the formula and fit the linear model 
#' for hedonic index calculations. 
#'
#' @param dataset A data.frame containing the variables.
#' @param dependent_variable A string specifying the name of the dependent variable.
#' @param independent_variables A character vector of all independent variables.
#' @return A fitted \code{lm} object.
#' @author Vivek Gajadhar
#' @importFrom stats as.formula lm
#' @keywords internal
#' @noRd
fit_hedonic_model <- function(dataset, dependent_variable, independent_variables) {
  
  # Construct the model formula
  model_formula <- stats::reformulate(
    termlabels = if (length(independent_variables) == 0) "1" else independent_variables,
    response = dependent_variable
  )
  
  # Fit the linear model
  model <- stats::lm(model_formula, data = dataset)
  
  return(model)
}

#' Predict from a Hedonic Model
#'
#' Centralized prediction helper for hedonic models.
#'
#' @param model A fitted model object.
#' @param newdata A data.frame to predict on.
#' @return A numeric vector of predictions.
#' @author Vivek Gajadhar
#' @importFrom stats predict
#' @keywords internal
#' @noRd
predict_hedonic <- function(model, newdata) {
  
  # Generate predictions based on the fitted model
  stats::predict(model, newdata = newdata)
}

#' Calculate imputation averages with the 1st period as base period
#'
#' Prices are estimated based on a provided Hedonic model
#' The model values are calculated for each period in the data
#' With these values, new prices of base period observations are estimated.
#' With this function, imputations according to the Laspeyres and Paasche method can be estimated.
#'
#' @author Farley Ishaak, Vivek Gajadhar
#' @param dataset_temp table with data 
#' @param period_temp 'period'
#' @param dependent_variable_temp usually the sale price
#' @param independent_variables_temp vector with quality determining variables
#' @param period_list_temp list with all available periods
#' @return
#' Table with imputation averages per period
#' @keywords internal
#' @noRd
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
  text_columns <- vapply(dataset_temp, function(column) {
    is.character(column) || is.factor(column)
  }, logical(1))
  for (column_name in names(dataset_temp)[text_columns]) {
    dataset_temp[[column_name]][dataset_temp[[column_name]] == ""] <- NA
  }
  dataset_temp <- dataset_temp[stats::complete.cases(dataset_temp), , drop = FALSE]
  
  # Remove unused levels. R remembers the original state of the levels, but if a level is not present in a certain period, this may result in an error in the bootstrap.
  dataset_temp <- droplevels(dataset_temp)
  
  # Prepare the dependent variable explicitly as a logged variable string for the helper
  dependent_variable_temp <- paste0("log(", dependent_variable_temp, ")")

  rows_by_period <- split(seq_len(nrow(dataset_temp)), dataset_temp[[period_temp]])

  # Empty vector for the values and numbers
  average_imputations <- numeric(number_of_periods)
  number_observations_total <- if (number_of_observations_temp) integer(number_of_periods) else NULL
  
  for (current_period in 1:number_of_periods) {
    
    # Estimate coefficients of the 1st period
    if (current_period == 1) {
      rekenbestand <- dataset_temp[rows_by_period[[period_list_temp[1]]], , drop = FALSE]
      
      # Use centralized helper for fitting
      fitmdl <- fit_hedonic_model(
        dataset = rekenbestand,
        dependent_variable = dependent_variable_temp,
        independent_variables = independent_variables_temp
      )
      
      if (number_of_observations_temp == TRUE) {
        number <- nrow(rekenbestand)
      }
    } else {
      # Estimate coefficients of all periods after
      rekenbestand_t <- dataset_temp[rows_by_period[[period_list_temp[current_period]]], , drop = FALSE]
      
      # Use centralized helper for fitting
      fitmdl <- fit_hedonic_model(
        dataset = rekenbestand_t,
        dependent_variable = dependent_variable_temp,
        independent_variables = independent_variables_temp
      )
      
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
    
    # Use centralized helper for prediction
    predictmdl_t <- mean(predict_hedonic(model = fitmdl, newdata = rekenbestand_0))
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
#' @author Farley Ishaak, Vivek Gajadhar
#' @param periods vector/variable with periods (numeric/string)
#' @param values vector/variable with to be transformed values (numeric)
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @return Index series
#' @keywords internal
#' @noRd
calculate_index <- function(periods, values, reference_period = NULL) {
  
  # Check length periods and values
  if (length(periods) != length(values)) {
    stop("The periods variable is not of the same length as the values variable.")
  }
  
  # Check numeric values
  if (!is.numeric(values)) {
    stop("The values variable is not (fully) numeric.")
  }
  
  # Transforms periods to characters
  periods <- as.character(periods)
  
  # If reference_period is not provided, then reference_period = 1st period from list
  if (is.null(reference_period)) {
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
  
  # Vectorized operation
  reference_values <- values[periods_short == reference_period]
  average <- mean(reference_values, na.rm = TRUE)
  
  # Calculate index
  index_series <- (values / average) * 100
  
  return(index_series)
}

#' Calculate Growth Rates
#'
#' Computes period-over-period growth rates from a numeric index vector.
#'
#' @param values A numeric vector representing index values.
#' @return A numeric vector of growth rates, with 1 as the initial value.
#' @author Vivek Gajadhar
#' @keywords internal
#' @importFrom utils head
#' @noRd

calculate_growth_rate <- function(values) {
  if (!is.numeric(values)) stop("The series of values is not fully numeric.")
  values <- as.numeric(values)
  growth_rate <- values / c(NA, head(values, -1))
  growth_rate[1] <- 1
  return(growth_rate)
}

#' Calculate the geometric average of a series of values
#'
#' The equation for the calculation is:: exp(mean(log(series_values)))
#'
#' @author Farley Ishaak 
#' @param values series with numeric values
#' @return geometric average
#' @keywords Internal
#' @noRd

calculate_geometric_average <- function(values){
  
  # Remove NA values
  values <- values[!is.na(values)]
  
  return(exp(mean(log(values))))
  
}






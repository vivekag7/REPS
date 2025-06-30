# initialize parameters to avoid "no visible binding for global variable" when running check()

utils::globalVariables(c("dataset", "dependent_variable", "continuous_variables", 
                         "independent_variables", 
                         "diagnostics", "period_list", 
                         "period_var_temp", "period", "value", "bind_rows", ".data","Index","method", "predicted_price"))

#' Validate Input Data for Hedonic Index Calculation
#'
#' This function checks whether the dataset contains all required variables, whether the dependent and continuous variables are numeric, 
#' and whether the period variable is formatted correctly (e.g., "2020Q1", "2020M01", or just "2015").
#' It also performs soft-matching to adjust a provided reference_period to align with the dataset.
#'
#' @param dataset A data.frame containing the dataset to be validated.
#' @param period_variable A string specifying the name of the period variable column.
#' @param dependent_variable A string specifying the name of the dependent variable (usually the sale price).
#' @param continuous_variables A character vector with names of numeric quality-determining variables.
#' @param categorical_variables A character vector with names of categorical variables (including dummies).
#' @param reference_period Optional string for the base period to normalize index values (e.g., "2015", "2020Q1").
#'
#' @return Returns TRUE invisibly if all checks pass. Automatically adjusts reference_period if needed.
#'
#' @author David Pietersz, Vivek Gajadhar
#' @keywords internal
#' 
#' @importFrom stringr str_detect

validate_input <- function(dataset, period_variable, dependent_variable, continuous_variables, categorical_variables, reference_period = NULL) {
  
  # Stop if both continuous and categorical variables are missing or empty
  if ((is.null(continuous_variables) || length(continuous_variables) == 0) &&
      (is.null(categorical_variables) || length(categorical_variables) == 0)) {
    stop("Both continuous_variables and categorical_variables are missing or empty. The model requires at least one type of explanatory variable to function.")
  }
  
  # Dataset must contain all required columns
  required_cols <- c(period_variable, dependent_variable, continuous_variables, categorical_variables)
  missing_cols <- required_cols[!required_cols %in% names(dataset)]
  if (length(missing_cols) > 0) {
    stop("Dataset is missing the following required column(s): ", paste(missing_cols, collapse = ", "))
  }
  
  # Dependent and continuous variables must be numeric
  numeric_cols <- c(dependent_variable, continuous_variables)
  for (col in numeric_cols) {
    if (!is.numeric(dataset[[col]])) {
      stop(paste("Column", col, "is not numeric."))
    }
  }
  
  # Dependent variable must contain strictly positive values for log transformation
  if (any(dataset[[dependent_variable]] <= 0, na.rm = TRUE)) {
    stop("The dependent variable contains zero or negative values, which is invalid if log transformation is to be applied.")
  }
  
  # Period variable should match standard formats for correct ordering and grouping
  regex_period <- "^([0-9]{4}([Mm](0?[1-9]|1[0-2])|[Qq](0?[1-4]))|[0-9]{6}|[0-9]{4})$"
  invalid_periods <- dataset[[period_variable]][!stringr::str_detect(dataset[[period_variable]], regex_period)]
  if (length(invalid_periods) > 0) {
    warning("The period variable contains values that do not match a recognized format.\nRecommended formats include: 2020Q1, 2020q1, 2020M1, 2020M01, 2020m01, 202001, or 2020.\nInvalid examples found: ", paste(unique(invalid_periods), collapse = ", "),
            "\nBe aware that sorting within the model may not be handled correctly for your period variables.")
  }
  
  # Handle reference_period
  if (!is.null(reference_period)) {
    actual_periods <- as.character(dataset[[period_variable]])
    reference_period <- as.character(reference_period)
    
    # Create shortened periods
    length_ref <- nchar(reference_period)
    periods_short <- substr(actual_periods, 1, length_ref)
    
    if (reference_period %in% actual_periods) {
    } else if (reference_period %in% periods_short) {
      warning("reference_period '", reference_period, "' not found exactly in data.\nUsing average of all values with periods starting with '", reference_period, "'.")
      
    } 
  }
  
  

}



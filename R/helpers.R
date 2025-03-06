### This is the first internal function

#' Calculate imputation averages with the 1th period as base period
#'
#' Prices are estimated based on a provided hedonic model
#' The model values are calculated for each period in the data
#' With these values, new prices of base period observations are estimated.
#' With this function, imputations according to the Laspeyres and Paasche method can be estimated.
#'
#' @author Farley Ishaak
#' @param dataset_temp = table with data (hoeft no selectie te zijn van benodigde variables)
#' @param period_temp = 'Period'
#' @param dependent_variable_temp = usually the sale price
#' @param independent_variables_temp = vector with quality determining variables
#' @param log_dependent_temp = should the dependent variable be transformed to its logarithm? default = TRUE
#' @param period_list_temp = list with all available periods
#' @return
#' table with imputation averages per period
#' @keywords internal

# the parameters worden (bij bedoeld gebruik) opgegeven in a bovenliggende functie
  
calculate_hedonic_imputation <- function(dataset_temp = dataset
                                         , period_temp = "period_var_temp"
                                         , dependent_variable_temp = dependent_variable
                                         , independent_variables_temp = independent_variables
                                         , log_dependent_temp = log_dependent
                                         , number_of_observations_temp = number_of_observations
                                         , period_list_temp = period_list) {

  ## Data operations

  # Count number of periods
  number_of_periods <- length(period_list_temp)

  # Select required variables
  # dataset_temp <- dataset_temp[, (names(dataset_temp) %in% c(period_temp, dependent_variable_temp, independent_variables_temp))]
  dataset_temp <- dataset_temp |>
    dplyr::select(all_of(c(period_temp, dependent_variable_temp,
                    independent_variables_temp)))

  # Remove lines without values
  ## this only check for missing data (empty cells). what if for example
  ## a numerical column has some non-numerical instances (faulty data).
  ## when observations are dropped, does this affect the way the index is computed?
  dataset_temp[dataset_temp == ''] <- NA
  dataset_temp <- stats::na.omit(dataset_temp)

  # Remove unused levels. R remembers the original state of the levels, but if a level is not present in a certain period, this may result in an error in the bootstrap.
  dataset_temp <- droplevels(dataset_temp)

  # If parameter log_dependent = TRUE, transform to log
  if (log_dependent_temp == TRUE) {
    dependent_variable_temp <- paste0("log(", dependent_variable_temp, ")")
  }

  ## Model

  # Compile model
  for (v in 1: length(independent_variables_temp)) {
    if (v==1) {
      model <- paste0(dependent_variable_temp, "~", independent_variables_temp[v])
    } else {
      model <- paste0(model, "+", independent_variables_temp[v])
    }
  }

  ## Calculate imputations per period

  # Empty vector for the values and numbers
  average_imputations <- c()
  number_observations_total <- c()

  for (p in 1:number_of_periods) {

    # Estimate coefficients of the 1th period
    if (p == 1) {
      rekenbestand <- subset(dataset_temp, period_var_temp == period_list_temp[1])
      fitmdl <- stats::lm(model,rekenbestand)
      predictmdl_0 <- mean(stats::predict(fitmdl, rekenbestand))

      # If parameter log_dependent = TRUE, then reverse with exp()
      if(log_dependent_temp == TRUE) {
        predictmdl_0 <- exp(predictmdl_0)
      }
      # If parameter number_of_observations = TRUE, then calculate numbers
      if (number_of_observations_temp == TRUE) {
        number <- nrow(rekenbestand)
      }
    } else {
      # Estimate coefficients of all periods after
      rekenbestand_t <- subset(dataset_temp, period_var_temp == period_list_temp[p])
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

    # If parameter log_dependent = TRUE, then reverse with exp()
    if (log_dependent_temp == TRUE) {
      predictmdl_t <- exp(predictmdl_t)
    }

    average_imputations[p] <- predictmdl_t

    # If parameter number_of_observations = TRUE, then add numbers to table
    if (number_of_observations_temp == TRUE) {
      number_observations_total[p] <- number
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
#' Thee vector should, therefore, also contain a mutation for the first period (this is likely 1).
#' In the calculation, this first mutation is not used.
#'
#' N.B. for the reference period:
#' The first value is on default set to 100.
#' An adjusted reference period can be provided in the paramater.
#' The reference period can also be a part of a period.
#' E.g. if the series contains months (2019jan, 2019feb), the reference period can be a year (2019).
#'
#' @author Farley Ishaak
#' @param periods = vector/variable with periods (numeric/string)
#' @param values = vector/variable with to be transformed values (numeric)
#' @param reference_period = period or group of periods that will be set to 100 (numeric/string)
#' @param from_growth_rate = is the index calculated from a growth rate (deviation from 1)? If not, then the index is calculated from a series of values (default = FALSE)
#' @return Index series

calculate_index <- function(periods
                            , values
                            , reference_period = NULL
                            , from_growth_rate = FALSE) {
  
  # Check length periods and values
  if (length(periods) != length(values)) {
    # stop("De reeks with periods is niet even lang als the reeks with values")
    stop("The periods variable is not of the same length as the values variable.")
  }
  
  # Check numeric values
  if (is.numeric(values) == FALSE) {
    # stop("De reeks with values is niet (volledig) numeric")
    stop("The values variable is not (fully) numeric.")
  }
  
  # Transforms periods to characters
  periods <- as.character(periods)
  
  # If reference_period is not provided, then reference_period = 1th period from list
  if (is.null(reference_period) == TRUE) {
    reference_period <- periods[1]
    periods_short <- periods
  } else {
    # Determine lengte reference_period
    length_reference_period <- nchar(reference_period)
    periods_short <- substr(periods, 1, length_reference_period)
  }
  
  # Check reference_period
  if (!(reference_period %in% periods_short)) {
    stop("The provided reference period is not part of the series with periods")
  }
  
  # Transform growth_rate to index
  if (from_growth_rate == TRUE) {
    values[1] <- 1
    values <- cumprod(values) * 100
  }
  
  # Create table
  tbl_index <- data.frame(period = periods_short, value = values)
  
  # Determine value reference_period
  # Average <- dplyr::filter(Tbl_index, period == reference_period)
  # Average <- dplyr::summarise(Average, Value = mean(Value))
  # Average <- as.vector(Average$Value)
  
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

#' Title
#'
#' @param single_iteration = a single iteration (usually 1 letter: i or p)
#' @param total_iterations = the total number of iterations in the loop
#'
#' @return this returns a progress text
#' @keywords internal
show_progress_loop <- function(single_iteration
                               , total_iterations){
  
  cat(sprintf("\rProgress: %3d%%", round(single_iteration / total_iterations * 100)))
  if (single_iteration == total_iterations) message("\n Done!")
  
}

### This is the fourth internal function

#' Calculate Confidence Interval Bounds
#'
#' This internal function calculates the confidence interval bounds based on bootstrap estimates.
#'
#' @param period A vector indicating the period associated with the data.
#' @param bootstraps An integer representing the number of bootstrap samples.
#' @param index The estimated index value.
#' @param sum The sum of bootstrap estimates.
#' @param sum_square The sum of squared bootstrap estimates.
#'
#' @return A data frame with the period, variance, lower bound, and upper bound of the confidence interval.
#' @keywords internal
calculate_bounds <- function(period, bootstraps, index, sum, sum_square) {
  # Calculate variance
  moment_1 <- sum / bootstraps
  moment_2 <- sum_square / bootstraps
  variance <- moment_2 - moment_1^2
  
  
  ### Ipv 1.96 hardcoderen (is niet precies), nu op basis van de
  ### kwantielen van de normale verdeling. Alpha staat nu standaard op 0.05
  alpha = 0.05
  probability <- 1 - (alpha / 2)
  
  # Calculate lower and upper bound of the confidence interval
  lower_bound <- index - stats::qnorm(probability) * sqrt(variance)
  upper_bound <- index + stats::qnorm(probability) * sqrt(variance)
  
  bounds <- data.frame(period = period, variance = variance, lower_bound = lower_bound,
                       upper_bound = upper_bound)
  
  return(bounds)
}



#' Calculate direct index according to the Fisher hedonic double imputation method
#'
#' By the parameters 'dependent_variable', 'continue_variable' and 'categorical_variables' as regression model is compiled.
#' With the model, a direct series of index figures is estimated by use of hedonic regression.
#'
#' N.B.: the independent variables must be entered transformed (and ready) in the parameters.
#' Hence, not: log(floor_area), but transform the variable in advance and then provide log_floor_area.
#' This does not count for the dependent variable. This should be entered untransformed/
#' The parameter log_dependent can be used to transform this variable.
#'
#' Within the data, it is not neccesary to filter the data on relevant variables or complete records.
#' This is taken care of in the function.
#'
#' @author Farley Ishaak (FIHK)
#' @param dataset table with data (does not need to be a selection of relevant variables)
#' @param period_variable variable in the table with periods
#' @param dependent_variable usually the sale price
#' @param continious_variables vector with quality determining numeric variables (no dummies)
#' @param categorical_variables vector with quality determining categorical variables (also dummies)
#' @param log_dependent should the dependent variable be transformed to its logarithm? default = TRUE
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param n_bootstraps the number of iterations for calculating a confidence interval (usually 500) (default = NULL -> no intervals)
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period
#' @export
#' @examples
#'
#' Tbl_Fisher <- calculate_fisher(dataset = data_constraxion
#'                                 , period_variable = c("period")
#'                                 , dependent_variable = c('price')
#'                                 , continious_variables = c('floor_area')
#'                                 , categorical_variables = c('neighbourhood_code')
#'                                 , log_dependent = TRUE
#'                                 , reference_period = 2015
#'                                 , number_of_observations = TRUE
#'                                 , n_bootstraps = 50)


calculate_fisher <- function(dataset
                             , period_variable
                             , dependent_variable
                             , continious_variables
                             , categorical_variables
                             , log_dependent = FALSE
                             , reference_period = NULL
                             , number_of_observations = FALSE
                             , n_bootstraps = NULL) {

  # Calculate Laspeyres with 1th period = 100
  laspeyres <- calculate_laspeyres(dataset = dataset
                                   , period_variable = period_variable
                                   , dependent_variable = dependent_variable
                                   , continious_variables = continious_variables
                                   , categorical_variables = categorical_variables
                                   , log_dependent = log_dependent
                                   , reference_period = NULL
                                   , index = TRUE
                                   , number_of_observations = number_of_observations
                                   , imputation = FALSE
                                   , n_bootstraps = NULL)

  # Calculate Paasche with 1th period = 100
  paasche <- calculate_paasche(dataset = dataset
                               , period_variable = period_variable
                               , dependent_variable = dependent_variable
                               , continious_variables = continious_variables
                               , categorical_variables = categorical_variables
                               , log_dependent = log_dependent
                               , reference_period = NULL
                               , index = TRUE
                               , number_of_observations = number_of_observations
                               , imputation = FALSE
                               , n_bootstraps = NULL)

  # Calculate Fisher (= geometric average)
  Index <- sqrt(laspeyres$Index * paasche$Index)

  # Rescale to reference_year
  Index <- calculate_index(laspeyres$period, Index, reference_period)

  ## Bootstrap
  if (!is.null(n_bootstraps)) {

    # Show progress
    print("Start Bootstrap")
    starting_time <- as.POSIXct(Sys.time())

    # Count number of skipped iterations
    skipped <- 0

    # Rename period_variable and transform to character
    # dataset <- dplyr::rename(dataset, Period_var_temp = period_variable)
    # dataset$Period_var_temp <- as.character(dataset$Period_var_temp)

    dataset <- dataset |>
      dplyr::rename(period_var_temp = period_variable) |>
      dplyr::mutate(period_var_temp = as.character(period_var_temp))

    # Create list of periods
    period_list <- sort(unique(dataset$period_var_temp), decreasing = FALSE)
    number_periods <- length(period_list)

    # Start at iteration 1
    current_bootstrap <- 1

    while (current_bootstrap <= n_bootstraps) {

      # Create a new dataset equally large to the original data
      for (current_period in 1:number_periods) { # per period a new dataset based on sample of original dataset
        subset <- which(dataset$period_var_temp == period_list[current_period]) # which records from original data have corresponding periods
        subset_length <- length(subset)

        bootstrapsubset <- subset[ceiling(stats::runif(subset_length) * subset_length)] # sample with return
        if (current_period == 1) bootstrapset <-  bootstrapsubset
        if (current_period > 1)  bootstrapset <- c(bootstrapset, bootstrapsubset)
      }
      dataset_bootstrap <- dataset[bootstrapset, ]

      # Check if the bootstrapset is equally large as the original dataset
      if (nrow(dataset_bootstrap) != nrow(dataset) || ncol(dataset_bootstrap) != ncol(dataset)) {
        stop("The number of rows and/or columns is not equal to the number in the original dataset.")
      }

      # Check if the bootstrapset per period is equally large to the original dataset
      length_dataset_period <- c(0)
      length_bootstrapped_dataset_period <- c(0)

      for (test_period in 1:number_periods) {
        test1 <- dataset[dataset$period_var_temp == period_list[test_period], ]
        test2 <- dataset_bootstrap[dataset_bootstrap$period_var_temp == period_list[test_period], ]
        length_dataset_period[test_period] <- nrow(test1)
        length_bootstrapped_dataset_period[test_period] <- nrow(test2)
      }
      if (sum(length_dataset_period == length_bootstrapped_dataset_period) != number_periods) {
        stop("The number is not equal in all periods")
      }

      # Test per period if each categorical variable has a minimum of 1 observation
      # If not: skip the bootstrap iteration

      # Reset the skip
      skip <- FALSE

      # for each categorical variable
      for (cat_variable in categorical_variables) {

        # Create a frequency table per period per level
        dataset_var <- table(dataset_bootstrap[, c("period_var_temp", cat_variable)])

        # Show an error if a cel has no observations
        if (0 %in% dataset_var) skip <- TRUE

      }

      # Skip this iteration and record how many are skipped
      if (skip == TRUE) {
        skipped <- skipped + 1
        next
      }

      # Calculate Laspeyres with 1th period = 100
      laspeyres_bootstrap <- calculate_laspeyres(dataset = dataset_bootstrap
                                                 , period_variable = "period_var_temp"
                                                 , dependent_variable = dependent_variable
                                                 , continious_variables = continious_variables
                                                 , categorical_variables = categorical_variables
                                                 , log_dependent = log_dependent
                                                 , reference_period = NULL
                                                 , index = TRUE
                                                 , number_of_observations = number_of_observations
                                                 , imputation = FALSE
                                                 , n_bootstraps = NULL)

      # Calculate Paasche with 1th period = 100
      paasche_bootstrap <- calculate_paasche(dataset = dataset_bootstrap
                                             , period_variable = "period_var_temp"
                                             , dependent_variable = dependent_variable
                                             , continious_variables = continious_variables
                                             , categorical_variables = categorical_variables
                                             , log_dependent = log_dependent
                                             , reference_period = NULL
                                             , index = TRUE
                                             , number_of_observations = number_of_observations
                                             , imputation = FALSE
                                             , n_bootstraps = NULL)

      # Calculate Fisher (= geometric average)
      index_bootstrap <- sqrt(laspeyres_bootstrap$Index * paasche_bootstrap$Index)

      # Rescale to reference_year
      index_bootstrap <- calculate_index(laspeyres_bootstrap$period, index_bootstrap, reference_period)

      # Calculate first and second moment: (EX)^2 and EX^2
      # if(current_bootstrap == 1)  Sum.square <- Index_bootstrap^2
      # if(current_bootstrap > 1) Sum.square <- Sum.square + (Index_bootstrap)^2
      #
      # if(current_bootstrap == 1)  Sum <- Index_bootstrap
      # if(current_bootstrap > 1) Sum <- Sum + Index_bootstrap


      if (current_bootstrap == 1)  sum_square <- index_bootstrap^2
      if (current_bootstrap > 1) sum_square <- sum_square + (index_bootstrap)^2

      if (current_bootstrap == 1)  sum <- index_bootstrap
      if (current_bootstrap > 1) sum <- sum + index_bootstrap

      # Show progress
      show_progress_loop(current_bootstrap, n_bootstraps)

      # Add one iteration
      current_bootstrap <- current_bootstrap + 1

    }

    # Calculate variance
    # M1 <- Sum/(bootstrap)
    # M2 <- Sum.square/(bootstrap)
    # variance <- M2 - M1^2
    #
    # # Calculate lower and upper bound of the confidence interval
    # Lower_bound <- Index - 1.96 * sqrt(variance)
    # Upper_bound <- Index + 1.96 * sqrt(variance)

    bounds <-  calculate_bounds(laspeyres$period, bootstraps = n_bootstraps, Index, sum, sum_square)

    print(paste0("Bootstrap is done! Duration: "
                 , round(difftime(as.POSIXct(Sys.time()), starting_time, units = "mins"), 0)
                 , " min. "))

  }

  # Create table
  fisher <- data.frame(period = laspeyres$period)

  if (number_of_observations == TRUE) {
    fisher$number_of_observations <- laspeyres$number_of_observations
  }

  fisher$Index <- Index

  if (!is.null(n_bootstraps)) {
    fisher$variance <- bounds$variance
    fisher$Lower_bound <- bounds$lower_bound
    fisher$Upper_bound <- bounds$upper_bound
  }

  return(fisher)

}

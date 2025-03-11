
#' Calculate direct index according to the Paasche hedonic double imputation method
#'
#' By the parameters 'dependent_variable', 'continue_variable' and 'categorical_variables' as regression model is compiled.
#' With the model, a direct series of index figures is estimated by use of hedonic regression.
#'
#' N.B.: the independent variables must be entered transformed (and ready) in the parameters.
#' Hence, not: log(floor_area), but transform the variable in advance and then provide log_floor_area.
#' This does not count for the dependent variable. This should be entered untransformed/
#' The parameter log_dependent can be used to transform this variable.
#'
#' Within the data, it is not necessary to filter the data on relevant variables or complete records.
#' This is taken care of in the function.
#'
#' @author Farley Ishaak (FIHK)
#' @param dataset table with data (does not need to be a selection of relevant variables)
#' @param period_variable variable in the table with periods
#' @param dependent_variable usually the sale price
#' @param continious_variables vector with quality determining numeric variables (no dummies)
#' @param categorical_variables vector with quality determining categorical variables (also dummies)
#' @param log_dependent should the dependent variable be transformed to its logarithm? default = TRUE
#' @param index caprice index
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param imputation display the underlying average imputation values? (default = FALSE)
#' @param n_bootstraps the number of iterations for calculating a confidence interval (usually 500) (default = NULL -> no intervals)
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period
#' @export
#' @examples
#'
#' Tbl_Paasche <- calculate_paasche(dataset = data_constraxion
#'                                 , period_variable = c("period")
#'                                 , dependent_variable = c('price')
#'                                 , continious_variables = c('floor_area')
#'                                 , categorical_variables = c('neighbourhood_code')
#'                                 , log_dependent = TRUE
#'                                 , index = TRUE
#'                                 , reference_period = 2015
#'                                 , number_of_observations = TRUE
#'                                 , imputation = TRUE
#'                                 , n_bootstraps = 50)


calculate_paasche <- function(dataset
                              , period_variable
                              , dependent_variable
                              , continious_variables
                              , categorical_variables
                              , log_dependent = FALSE
                              , reference_period = NULL
                              , index = TRUE
                              , number_of_observations = FALSE
                              , imputation = FALSE
                              , n_bootstraps = NULL) {

  # Merge independent variables
  # independent_variables <- c(continious_variables, categorical_variables)
  assertthat::assert_that(assertthat::has_name(dataset, c(period_variable, dependent_variable, continious_variables, categorical_variables)))
  independent_variables <- c(continious_variables, categorical_variables)

  # Rename period_variable and transform to character
  dataset <- dataset |>
    dplyr::rename(period_var_temp = period_variable) |>
    dplyr::mutate(period_var_temp = as.character(period_var_temp),
                  dplyr::across(dplyr::all_of(categorical_variables),
                                as.factor))

  # dataset <- dplyr::rename(dataset, Period_var_temp = period_variable)
  # dataset$Period_var_temp <- as.character(dataset$Period_var_temp)

  # Data processing categorical variables
  # dataset <- dplyr::mutate(dataset, dplyr::across(dplyr::all_of(categorical_variables), as.factor)) # Transform columns to factor


  ## Calculate index

  # Create list of periods
  period_list <- sort(unique(dataset$period_var_temp), decreasing = FALSE)
  number_of_periods <- number_of_periods_temp <- length(period_list)

  # Prepare table for imputations
  tbl_imputations <- data.frame(period = period_list)

  # Prepare vector for index and numbers
  Index <- c(0)
  number <- c(0)

  ### w -> imputation_period
  ### w 2x gebruikt als variabele, ook bij bootstrap gedeelte.
  for (imputation_period in 1:number_of_periods) {

    # Select the last and first period
    period_list_paasche <- c(period_list[number_of_periods_temp], period_list[1])
    dataset_temp <- dataset[which(dataset$period_var_temp %in% period_list_paasche), ]

    # Calculate Paasche imputations and numbers
    tbl_average_imputation <-
      calculate_hedonic_imputation(dataset_temp = dataset_temp
                                   , period_temp = "period_var_temp"
                                   , dependent_variable_temp = dependent_variable
                                   , independent_variables_temp = independent_variables
                                   , log_dependent_temp = log_dependent
                                   , number_of_observations_temp = number_of_observations
                                   , period_list_temp = period_list_paasche)

    if (imputation == TRUE) {

      # Insert imputations into table
      tbl_imputations <- merge(tbl_imputations, tbl_average_imputation, "period", all.x = TRUE)
      # tbl_imputations <- tbl_imputations |>
      #   dplyr::left_join(tbl_average_imputation, by = "period")

      # Rename variable to base year
      names(tbl_imputations)[ncol(tbl_imputations)] <- paste0("Base_", period_list[number_of_periods_temp])
    }

    if (number_of_observations == TRUE) {

      # Insert imputations into table
      number[imputation_period] <- tbl_average_imputation$number_of_observations[1]

    }

    # Insert last index figure into vector
    Index[imputation_period] <- tbl_average_imputation$average_imputation[1] / tbl_average_imputation$average_imputation[2] * 100

    # Stepwise delete last period
    number_of_periods_temp <- number_of_periods_temp - 1

  }

  # Reverse the index series (last period was calculated first)
  Index <- Index[imputation_period:1]
  number <- number[imputation_period:1]

  # Rescale the index
  # if (is.null(reference_period) == FALSE) {
  if (!is.null(reference_period)) {
    Index <- calculate_index(period_list, Index, reference_period)
  }

  ## Bootstrap
  # if (is.null(n_bootstraps) == FALSE) {
  if (!is.null(n_bootstraps)) {

    # Show progress
    print("Start Bootstrap")
    starting_time <- as.POSIXct(Sys.time())

    # Count number of skipped iterations
    skipped <- 0

    # Start at iteration 1
    current_bootstrap <- 1

    while (current_bootstrap <= n_bootstraps) {

      # Create a new dataset equally large to the original data
      for (bootstrap_period in 1:number_of_periods) { #  per period a new dataset based on sample of original dataset
        subset <- which(dataset$period_var_temp == period_list[bootstrap_period]) # which records from original data have corresponding periods
        subset_length <- length(subset)

        bootstrapsubset <- subset[ceiling(stats::runif(subset_length) * subset_length)] # sample with return
        if (bootstrap_period == 1) bootstrapset <-  bootstrapsubset
        if (bootstrap_period > 1)  bootstrapset <- c(bootstrapset, bootstrapsubset)
      }

      dataset_bootstrap <- dataset[bootstrapset, ]

      # Check if the bootstrapset is equally large as the original dataset
      # if (nrow(dataset_bootstrap) != nrow(dataset) || ncol(dataset_bootstrap) != ncol(dataset)) {
      #   print("Error: the number of rows and/or columns is not equal to the number in the original dataset.")
      # }

      if (nrow(dataset_bootstrap) != nrow(dataset) ||
          ncol(dataset_bootstrap) != ncol(dataset)) {
        ## Dit print een error message, maar de berekning gaat door. aangepast naar stop(..)
        # print("Error: the number of rows and/or columns is not equal to the number in the original dataset.")
        stop(paste0("The number of rows and/or columns is not equal",
                    "to the number in the original dataset."))
      }

      # Check if the bootstrapset per period is equally large to the original dataset
      # n <-c(0)
      # n.BS<-c(0)

      length_dataset_period <- c(0)
      length_bootstrapped_dataset_period <- c(0)

      for (test_period in 1:number_of_periods) {
        test1 <- dataset[dataset$period_var_temp == period_list[test_period], ]
        test2 <- dataset_bootstrap[dataset_bootstrap$period_var_temp == period_list[test_period], ]
        length_dataset_period[test_period] <- nrow(test1)
        length_bootstrapped_dataset_period[test_period] <- nrow(test2)
      }
      if (sum(length_dataset_period == length_bootstrapped_dataset_period) != number_of_periods) {
        # print("Error: the number is not equal in all periods")
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
        if (0 %in% dataset_var) {
          skip <- TRUE
        }
      }

      # Skip this iteration and record how many are skipped
      if (skip == TRUE) {
        skipped <- skipped + 1
        next
      }

      ## Index calculation

      # Prepare table for imputations
      ## Dit wordt nergens gebruikt.
      # Tbl_Imputations_bootstrap <- data.frame(Period = period_list)

      # Prepare vector for index and numbers
      index_bootstrap <- c(0)

      # Reset the number of periods
      number_of_periods_temp <- number_of_periods

      ## w -> bootstrap_period
      for (bootstrap_period in 1:number_of_periods){

        # Select the last and first period
        period_list_paasche <- c(period_list[number_of_periods_temp], period_list[1])
        dataset_bootstrap_temp <- dataset_bootstrap[which(dataset_bootstrap$period_var_temp %in% period_list_paasche), ]

        # Calculate Paasche imputations and numbers
        tbl_average_imputation_bootstrap <-
          calculate_hedonic_imputation(dataset_temp = dataset_bootstrap_temp
                                       , period_temp = "period_var_temp"
                                       , dependent_variable_temp = dependent_variable
                                       , independent_variables_temp = independent_variables
                                       , log_dependent_temp = log_dependent
                                       , number_of_observations_temp = number_of_observations
                                       , period_list_temp = period_list_paasche)

        # Insert last index figure into vector
        index_bootstrap[bootstrap_period] <- tbl_average_imputation_bootstrap$average_imputation[1] / tbl_average_imputation_bootstrap$average_imputation[2] * 100

        # Stepwise delete last period
        number_of_periods_temp <- number_of_periods_temp - 1
      }

      # Reverse the index series (last period was calculated first)
      index_bootstrap <- index_bootstrap[imputation_period:1]

      # Rescale the index
      if (is.null(reference_period) == FALSE) {
        index_bootstrap <- calculate_index(period_list, index_bootstrap, reference_period)
      }

      # Calculate first and second moment: (EX)^2 and EX^2
      # if(h == 1)  Sum.square <- Index_bootstrap^2
      # if(h > 1) Sum.square <- Sum.square + (Index_bootstrap)^2
      #
      # if(h == 1)  Sum <- Index_bootstrap
      # if(h > 1) Sum <- Sum + Index_bootstrap

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

    bounds <-  calculate_bounds(tbl_average_imputation$period, bootstraps = n_bootstraps, Index, sum, sum_square)

    print(paste0("Bootstrap is done! Duration: "
                 , round(difftime(as.POSIXct(Sys.time()), starting_time, units = "mins"), 0)
                 , " min. "))

  }

  # Create table
  paasche <- data.frame(period = period_list)
  column_start <- 1

  if (number_of_observations == TRUE) {
    paasche$number_of_observations <- number
    column_start <- 2
  }
  if (index == TRUE) {
    paasche$Index <- Index
  }
  if (is.null(n_bootstraps) == FALSE) {
    # Paasche$variance <- variance
    # Paasche$Lower_bound <- Lower_bound
    # Paasche$Upper_bound <- Upper_bound
    paasche$variance <- bounds$variance
    paasche$lower_bound <- bounds$lower_bound
    paasche$upper_bound <- bounds$upper_bound
  }
  if (imputation == TRUE) {
    number_of_periods_plus_1 <- number_of_periods + 1
    tbl_imputations <- tbl_imputations[, c(1, number_of_periods_plus_1:column_start)] # Reverse the table (last period was calculated first)
    tbl_imputations <- unique.data.frame(tbl_imputations) # the base period was doubled
    paasche <- merge(paasche, tbl_imputations, by = "period")
  }

  return(paasche)
}

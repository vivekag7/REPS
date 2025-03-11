# This is the first main function (calculate laspeyres)

#' Calculate direct index according to the Laspeyres hedonic double imputation method
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
#'
#' @param dataset table with data (does not need to be a selection of relevant variables)
#' @param period_variable variable in the table with periods
#' @param dependent_variable usually the sale price
#' @param continious_variables vector with quality determining numeric variables (no dummies)
#' @param categorical_variables vector with quality determining categorical variables (also dummies)
#' @param log_dependent should the dependent variable be transformed to its logarithm? default = TRUE
#' @param reference_period period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations number of observations per period (default = TRUE)
#' @param imputation display the inderlying average imputation values? (default = FALSE)
#' @param index caprice index
#' @param n_bootstraps the number of iterations for calculating a confidence interval (usually 500) (default = NULL -> no intervals)
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr lag
#' @importFrom stats na.omit
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom stats runif
#' @importFrom assertthat assert_that
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period
#' @export
#' @examples
#'
#' Tbl_Laspeyres <- calculate_laspeyres(dataset = data_constraxion
#'                                 , period_variable = c("period")
#'                                 , dependent_variable = c('price')
#'                                 , continious_variables = c('floor_area')
#'                                 , categorical_variables = c('neighbourhood_code')
#'                                 , log_dependent = TRUE
#'                                 , reference_period = 2015
#'                                 , number_of_observations = TRUE
#'                                 , imputation = TRUE
#'                                 , n_bootstraps = 50)
calculate_laspeyres <- function(dataset
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
  ## bootstraps vervangen door n_bootstraps voor meer duidelijkheid dat het om een nummer gaat en niet wel/niet bootstrap uitvoeren.
  
  ### Controle toegevoegd om te kijken of aangewezen variabelen in de dataset
  ### voorkomen. Dit kan ook in een functie gedaan worden, aangezien het het ook voorkomt in paasche en fischer.
  # Merge independent variables
  # col_names <- names(dataset)
  # if (is.character(continious_variables) &&
  #     is.character(categorical_variables)) {
  #   independent_variables <- c(continious_variables, categorical_variables)
  #   if (!all(independent_variables %in% col_names) ||
  #       !(dependent_variable %in% col_names)) {
  #     stop("One or more of the variables is not present in the dataset.")
  #   }
  # } else {
  #   stop(paste("The input for the continious and categorical names",
  #               "must be the name of the columns in the dataframe."))
  # }
  
  ## assertthat voert alle hieboven beschreven checks uit.
  ## Check als dependent variable numeriek is? Ook voor cont vars?
  assertthat::assert_that(assertthat::has_name(dataset, c(period_variable, dependent_variable, continious_variables, categorical_variables)))
  independent_variables <- c(continious_variables, categorical_variables)
  
  
  # Rename period_variable and transform to character
  ### Omgezet naar tidyverse-style, een mix tussen base en tidyverse is inconsistent.
  dataset <- dataset |>
    dplyr::rename(period_var_temp = period_variable) |>
    dplyr::mutate(period_var_temp = as.character(period_var_temp),
                  dplyr::across(dplyr::all_of(categorical_variables),
                                as.factor))
  
  # dataset <- dplyr::rename(dataset, Period_var_temp = period_variable)
  # dataset$Period_var_temp <- as.character(dataset$Period_var_temp)
  #
  # # Data processing categorical variables
  # dataset <- dplyr::mutate(dataset, dplyr::across(dplyr::all_of(categorical_variables),
  #                                        as.factor)) # Transform columns to factor
  
  # Create list of periods
  period_list <- sort(unique(dataset$period_var_temp), decreasing = FALSE)
  
  # Calculate laspeyres imputations and numbers
  tbl_average_imputation <-
    calculate_hedonic_imputation(dataset_temp = dataset
                                 , period_temp = "period_var_temp"
                                 , dependent_variable_temp = dependent_variable
                                 , independent_variables_temp = independent_variables
                                 , log_dependent_temp = log_dependent
                                 , number_of_observations_temp = number_of_observations
                                 , period_list_temp = period_list)
  
  
  # Calculate index
  Index <- calculate_index(tbl_average_imputation$period, tbl_average_imputation$average_imputation, reference_period = reference_period)
  
  ## Bootstrap
  ### Aangepast in meer R style
  if (!is.null(n_bootstraps)) {
    # if (is.null(bootstrap) == FALSE) {
    
    # Show progress
    print("Start Bootstrap")
    starting_time <- as.POSIXct(Sys.time())
    
    # Count number of skipped iterations
    skipped <- 0
    
    # Start at iteration 1
    # h <- 1
    current_bootstrap <- 1
    
    while (current_bootstrap <= n_bootstraps) {
      
      # Create a new dataset equally large to the original data
      periods <- tbl_average_imputation$period
      number_periods <- length(periods)
      
      ### p is not a good variable name.
      ### something else then current_period?
      for (current_period in 1:number_periods) { # per period a new dataset based on sample of original dataset
        
        subset <- which(dataset$period_var_temp == periods[current_period]) # which records from original data have corresponding periods
        ### variabele naam n aangepast
        # n <- length(subset)
        subset_length <- length(subset)
        
        ## added stats::
        bootstrapsubset <- subset[ceiling(stats::runif(subset_length) * subset_length)] # sample with return
        if (current_period == 1) {
          bootstrapset <-  bootstrapsubset
        }
        if (current_period > 1) {
          bootstrapset <- c(bootstrapset, bootstrapsubset)
        }
      }
      
      dataset_bootstrap <- dataset[bootstrapset, ]
      
      # Check if the bootstrapset is equally large as the original dataset
      if (nrow(dataset_bootstrap) != nrow(dataset) ||
          ncol(dataset_bootstrap) != ncol(dataset)) {
        ## Dit print een error message, maar de berekning gaat door. aangepast naar stop(..)
        # print("Error: the number of rows and/or columns is not equal to the number in the original dataset.")
        stop(paste0("The number of rows and/or columns is not equal",
                    "to the number in the original dataset."))
      }
      
      # Check if the bootstrapset per period is equally large to the original dataset
      ### hoofdletters weggehaald en "." vervangen voor "_".  variabele n wordt al elderes gebruikt (hierboven). namen aangepast.
      # n <-c(0)
      # n.BS<-c(0)
      
      length_dataset_period <- c(0)
      length_bootstrapped_dataset_period <- c(0)
      
      
      for (test_period in 1:number_periods) {
        ######  "a" geen goede variabelenaam.
        test_1 <- dataset[dataset$period_var_temp == periods[test_period], ]
        test_2 <- dataset_bootstrap[dataset_bootstrap$period_var_temp == periods[test_period], ]
        length_dataset_period[test_period] <- nrow(test_1)
        length_bootstrapped_dataset_period[test_period] <- nrow(test_2)
      }
      
      ##### Statement that affect control flow should go in their own {} block
      if (sum(length_dataset_period == length_bootstrapped_dataset_period) != number_periods) {
        # print("error: the number is not equal in all periods")
        stop("Error: the number is not equal in all periods.")
      }
      
      # Test per period if each categorical variable has a minimum of 1 observation
      # If not: skip the bootstrap iteration
      
      # Reset the skip
      # skip omgezet naar een boolean -> consistenter.
      skip <- FALSE
      
      # for each categorical variable
      ##### "i" vervangen voor cat_variable
      for (cat_variable in categorical_variables) {
        
        # Create a frequency table per period per level
        ### dataset.var is geen snake_case stijl. vervangen
        dataset_var <- table(dataset_bootstrap[,c("period_var_temp", cat_variable)])
        
        # Show an error if a cel has no observations
        #### moet hier een error message verschijnen? Dat gebeurt nu niet.
        if (0 %in% dataset_var) {
          skip <- TRUE
        }
      }
      
      # Skip this iteration and record how many are skipped
      if (skip == TRUE) {
        skipped <- skipped + 1
        next
      }
      
      # Calculate laspeyres imputations and numbers
      tbl_average_imputation_bootstrap <-
        calculate_hedonic_imputation(dataset_temp = dataset_bootstrap
                                     , period_temp = "period_var_temp"
                                     , dependent_variable_temp = dependent_variable
                                     , independent_variables_temp = independent_variables
                                     , log_dependent_temp = log_dependent
                                     , number_of_observations_temp = FALSE
                                     , period_list_temp = period_list)
      
      # Calculate index
      index_bootstrap <- calculate_index(tbl_average_imputation_bootstrap$period
                                         , tbl_average_imputation_bootstrap$average_imputation
                                         , reference_period = reference_period)
      
      # Calculate first and second moment: (EX)^2 and EX^2
      #### variabelenaam "h" aangepast. hoofdletters weggehaald.
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
    
    ##### Deze berekening wordt ook elders gebruikt, dus ik heb hem in een functie
    ##### gestopt (calculate_bounds).
    # Calculate variance
    # moment_1 <- sum / (bootstrap)
    # moment_2 <- sum_square / (bootstrap)
    # variance <- moment_2 - moment_1^2
    #
    # # Calculate lower and upper bound of the confidence interval
    # lower_bound <- Index - 1.96 * sqrt(variance)
    # upper_bound <- Index + 1.96 * sqrt(variance)
    
    bounds <-  calculate_bounds(tbl_average_imputation$period, bootstraps = n_bootstraps, Index, sum, sum_square)
    
    print(paste0("Bootstrap is done! Duration: ",
                 round(difftime(as.POSIXct(Sys.time()), starting_time, units = "mins"), 0),
                 " min. "))
    
  }
  
  # Create table
  laspeyres <- data.frame(period = tbl_average_imputation$period)
  
  if (number_of_observations == TRUE) {
    laspeyres$number_of_observations <- tbl_average_imputation$number_of_observations
  }
  if (imputation == TRUE) {
    laspeyres$Imputation <- tbl_average_imputation$average_imputation
  }
  if (index == TRUE) {
    laspeyres$Index <- Index
  }
  if (!is.null(n_bootstraps)) {
    # laspeyres$variance <- variance
    # laspeyres$lower_bound <- lower_bound
    # laspeyres$upper_bound <- upper_bound
    laspeyres$variance <- bounds$variance
    laspeyres$lower_bound <- bounds$lower_bound
    laspeyres$upper_bound <- bounds$upper_bound
  }
  
  return(laspeyres)
  
}
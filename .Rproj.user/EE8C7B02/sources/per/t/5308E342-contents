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
#' @param dataset = table with data (does not need to be a selection of relevant variables)
#' @param period_variable = variable in the table with periods
#' @param dependent_variable = usually the sale price
#' @param continious_variables = vector with quality determining numeric variables (no dummies)
#' @param categorical_variables = vector with quality determining categorical variables (also dummies)
#' @param log_dependent = should the dependent variable be transformed to its logarithm? default = TRUE
#' @param reference_period = period or group of periods that will be set to 100 (numeric/string)
#' @param number_of_observations = number of observations per period (default = TRUE)
#' @param imputation = display the inderlying average imputation values? (default = FALSE)
#' @param index = Caprice index
#' @param bootstrap = the number of iterations for calculating a confidence interval (usually 500) (default = NULL -> no intervals)
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
#' @return
#' table with index, imputation averages, number of observations and confidence intervals per period
#' @export
#' @examples
#'
#' Tbl_Laspeyres <- Calculate_laspeyres(dataset = data_constraxion
#'                                 , period_variable = c("period")
#'                                 , dependent_variable = c('price')
#'                                 , continious_variables = c('floor_area')
#'                                 , categorical_variables = c('neighbourhood_code')
#'                                 , log_dependent = TRUE
#'                                 , reference_period = 2015
#'                                 , number_of_observations = TRUE
#'                                 , imputation = TRUE
#'                                 , bootstrap = 50)
Calculate_laspeyres <- function(dataset
                                , period_variable
                                , dependent_variable
                                , continious_variables
                                , categorical_variables
                                , log_dependent = FALSE
                                , reference_period = NULL
                                , index = TRUE
                                , number_of_observations = FALSE
                                , imputation = FALSE
                                , bootstrap = NULL){

  # Merge independent variables
  independent_variables <- c(continious_variables, categorical_variables)

  # Rename period_variable and transform to character

  dataset <- dplyr::rename(dataset, Period_var_temp = period_variable)
  dataset$Period_var_temp <- as.character(dataset$Period_var_temp)

  # Data processing categorical variables
  dataset <- dplyr::mutate(dataset, dplyr::across(dplyr::all_of(categorical_variables), as.factor)) # Transform columns to factor

  # Create list of periods
  period_list <- sort(unique(dataset$Period_var_temp), decreasing = FALSE)

  # Calculate Laspeyres imputations and numbers
  Tbl_average_imputation <-
    calculate_hedonic_imputation(dataset_temp = dataset
                                 , Period_temp = 'Period_var_temp'
                                 , dependent_variable_temp = dependent_variable
                                 , independent_variables_temp = independent_variables
                                 , log_dependent_temp = log_dependent
                                 , number_of_observations_temp = number_of_observations
                                 , period_list_temp = period_list)


  # Calculate index
  Index <- calculate_index(Tbl_average_imputation$Period, Tbl_average_imputation$average_imputation, reference_period = reference_period)

  ## Bootstrap
  if(is.null(bootstrap) == FALSE){

    # Show progress
    print("Start Bootstrap")


    # Count number of skipped iterations
    skipped <- 0

    # Start at iteration 1
    h <- 1

    while (h <= bootstrap) {

      # Create a new dataset equally large to the original data
      periods <- Tbl_average_imputation$Period
      number_periods <- length(periods)

      for(p in 1:number_periods) # per period a new dataset based on sample of original dataset
      {
        subset <- which(dataset$Period_var_temp==periods[p]) # which records from original data have corresponding periods
        n <- length(subset)

        bootstrapsubset <- subset[ceiling(runif(n)*n)] # sample with return
        if(p==1) bootstrapset <-  bootstrapsubset
        if(p>1)  bootstrapset <- c(bootstrapset, bootstrapsubset)
      }
      dataset_bootstrap <- dataset[bootstrapset,]

      # Check if the bootstrapset is equally large as the original dataset
      if(nrow(dataset_bootstrap)!=nrow(dataset) | ncol(dataset_bootstrap)!=ncol(dataset)) print("Error: the number of rows and/or columns is not equal to the number in the original dataset.")

      # Check if the bootstrapset per period is equally large to the original dataset
      n <-c(0)
      n.BS<-c(0)
      for (a in 1:number_periods){
        test1 <- dataset[dataset$Period_var_temp==periods[a],]
        test2 <- dataset_bootstrap[dataset_bootstrap$Period_var_temp==periods[a],]
        n[a] <- nrow(test1)
        n.BS[a] <- nrow(test2)
      }
      if(sum(n == n.BS)!=number_periods) print("error: the number is not equal in all periods")

      # Test per period if each categorical variable has a minimum of 1 observation
      # If not: skip the bootstrap iteration

      # Reset the skip
      skip <- "No"

      # for each categorical variable
      for (i in categorical_variables){

        # Create a frequency table per period per level
        dataset.var <- table(dataset_bootstrap[,c("Period_var_temp",i)])

        # Show an error if a cel has no observations
        if(0 %in% dataset.var) skip <- "Yes"

      }

      # Skip this iteration and record how many are skipped
      if (skip == "Yes"){
        skipped <- skipped + 1
        next
      }

      # Calculate Laspeyres imputations and numbers
      Tbl_average_imputation_bootstrap <-
        calculate_hedonic_imputation(dataset_temp = dataset_bootstrap
                                     , Period_temp = 'Period_var_temp'
                                     , dependent_variable_temp = dependent_variable
                                     , independent_variables_temp = independent_variables
                                     , log_dependent_temp = log_dependent
                                     , number_of_observations_temp = FALSE
                                     , period_list_temp = period_list)

      # Calculate index
      Index_bootstrap <- calculate_index(Tbl_average_imputation_bootstrap$Period
                                         , Tbl_average_imputation_bootstrap$average_imputation
                                         , reference_period = reference_period)

      # Calculate first and second moment: (EX)^2 and EX^2
      if(h == 1)  Sum.square <- Index_bootstrap^2
      if(h > 1) Sum.square <- Sum.square + (Index_bootstrap)^2

      if(h == 1)  Sum <- Index_bootstrap
      if(h > 1) Sum <- Sum + Index_bootstrap

      # Show progress
      show_progress_loop(h, bootstrap)

      # Add one iteration
      h <- h + 1

    }

    # Calculate variance
    M1 <- Sum/(bootstrap)
    M2 <- Sum.square/(bootstrap)
    variance <- M2 - M1^2

    # Calculate lower and upper bound of the confidence interval
    Lower_bound <- Index - 1.96 * sqrt(variance)
    Upper_bound <- Index + 1.96 * sqrt(variance)


  }

  # Create table
  Laspeyres <- data.frame(Period = Tbl_average_imputation$Period)

  if(number_of_observations == TRUE){
    Laspeyres$number_of_observations = Tbl_average_imputation$number_of_observations
  }
  if(imputation == TRUE){
    Laspeyres$Imputation = Tbl_average_imputation$average_imputation
  }
  if(index == TRUE){
    Laspeyres$Index <- Index
  }
  if(is.null(bootstrap) == FALSE){
    Laspeyres$variance <- variance
    Laspeyres$Lower_bound <- Lower_bound
    Laspeyres$Upper_bound <- Upper_bound
  }

  return(Laspeyres)

}

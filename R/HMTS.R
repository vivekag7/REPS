
#' Calculate HMTS index with bootstrap (Hedonic Multilateral Time series re-estimation Splicing)
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
#' Untill this period, all periods are imputed. After that, 1 period is added.
#'
#' Parameter 'resting_points':
#' If TRUE, the output is a list of tables. These tables can be called with a $ after the output.
#' $Index = table with periods, index and number of observations
#' $Window = table with the index figures within the chosen window
#' $Chosen_index_series = table with index series before the window splice
#' $Matrix_HMTS_index = table with index series based on re-estimated imputations (time series model)
#' $Matrix_HMTS = table with re-estimated imputations (time series model)
#' $Matrix_HMTS_index = table with index series based on estimated imputations (hedonic model)
#' $Matrix_HMTS = table with estimated imputations (time series model)l
#' $Matrix_HMTS_analyse = table with diagnostic values of the time series model per base period
#'
#' @author Farley Ishaak
#' @param period_variable = variable in the dataset with the period
#' @param dependent_variable = usually the sale price
#' @param continious_variables = vector with quality-determining continues variables (numeric, no dummies)
#' @param categorical_variables = vector with categorical variables (also dummy)
#' @param log_dependent = should the dependent variable be transformed to its logarithm? default = TRUE
#' @param reference_period = period or group of periods that will be set to 100 (numeric/string)
#' @param state_space_model = the model number of the trend calculation (default = 5 -> smooth). No trend = NULL
#' @param number_of_observations = number of observations per period (default = TRUE)
#' @param periods_in_year = if month, then 12. If quarter, then 4, etc. (default = 4)
#' @param production_since = 1 period in the format of the period_variable. See description above (default = NULL)
#' @param number_preliminary_periods = number of periods that the index is preliminary. Only works if production_since <> NULL. default = 3
#' @param resting_points = Should analyses values be returned? (default = FALSE)
#' @param bootstrap = the number of iterations for calculating a confidence interval (usually 500) (default = NULL -> no intervals)
#' @importFrom stats qnorm
#' @importFrom stats window
#' @return
#' $Matrix_HMTS_index = table with index series based on estimations with time series re-estimations
#' $Matrix_HMTS = table with estimated values based on time series re-estimations
#' $Matrix_HMS_index = table with index series based on estimations with the hedonic model
#' $Matrix_HMS = table with estimated values based on the hedonic model
#' $Matrix_HMTS_analysis = table with analysis values of the time series model per base period
#' @keywords internal
#' @return table with periods, index (and optional confidence intervals) and number of observations. If resting_points = TRUE, then list with tables. See general description and examples.
#' @export
#' @examples
#' HMTS <- calculate_HMTS(
#'       dataset = data_constraxion
#'       , period_variable = c("period")
#'       , dependent_variable = c('price')
#'       , continious_variables = c('floor_area')
#'       , categorical_variables = c('neighbourhood_code')
#'       , log_dependent = TRUE
#'       , reference_period = NULL
#'       , state_space_model = 5
#'       , periods_in_year = 4
#'       , production_since = '2008Q1'
#'       , number_preliminary_periods = 2
#'       , resting_points = TRUE
#'       , bootstrap = 20)
#'
#' Tbl_index <- as.data.frame(HMTS$Index)
#' Tbl_analysis <- as.data.frame(HMTS$Matrix_HMTS_analysis)

calculate_HMTS <- function(
    dataset,
    period_variable,
    dependent_variable,
    continious_variables,
    categorical_variables,
    log_dependent = TRUE,
    reference_period = NULL,
    state_space_model = 5,
    periods_in_year = 4,
    production_since = 201404,
    number_preliminary_periods = 1,
    number_of_observations = TRUE,
    resting_points = FALSE,
    bootstrap = NULL) {

  # Calculate index
  Results <- calculate_HMTS_index(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    continious_variables = continious_variables,
    categorical_variables = categorical_variables,
    log_dependent = log_dependent,
    reference_period = reference_period,
    state_space_model = state_space_model,
    periods_in_year = periods_in_year,
    production_since = production_since,
    number_preliminary_periods = number_preliminary_periods,
    number_of_observations = number_of_observations,
    resting_points = resting_points,
    bootstrap = NULL)

  # Store resting points
  if(resting_points == TRUE){
    Tbl_resting_points <- Results
    Results <- as.data.frame(Results$Index)
  }

  ## Bootstrap
  if(is.null(bootstrap) == FALSE){

    # Temporarily turn off warnings
    defaultW <- getOption("warn")
    options(warn = -1)

    # Show progress
    print("Start Bootstrap")
    starting_time <- as.POSIXct(Sys.time())

    # Count number of skipped iterations
    skipped <- 0

    # Rename period_variable and transform to character
    dataset <- dplyr::rename(dataset, Period_var_temp = period_variable)
    dataset$Period_var_temp <- as.character(dataset$Period_var_temp)

    # Count number of periods
    period_list <- sort(unique(dataset$Period_var_temp))
    number_of_periods <- length(period_list)

    # Start at iteration 1
    h <- 1

    while (h <= bootstrap) {

      # Create a new dataset equally large to the original data
      for(p in 1:number_of_periods) # per period a new dataset based on sample of original dataset
      {
        subset <- which(dataset$Period_var_temp==period_list[p]) # which records from original data have corresponding periods
        n <- length(subset)

        bootstrapsubset <- subset[ceiling(runif(n)*n)] # sample with return
        if(p==1) bootstrapset <-  bootstrapsubset
        if(p>1)  bootstrapset <- c(bootstrapset, bootstrapsubset)
      }
      dataset_bootstrap <- dataset[bootstrapset,]

      # Check if the bootstrapset is equally large as the original dataset
      if(nrow(dataset_bootstrap)!=nrow(dataset) | ncol(dataset_bootstrap)!=ncol(dataset)) print("Error: the number of rows and/or columns is not equal to the number in the original dataset.")

      # Check if the bootstrapset is equally large as the original dataset
      n <-c(0)
      n.BS<-c(0)
      for (a in 1:number_of_periods){
        test1 <- dataset[dataset$Period_var_temp==period_list[a],]
        test2 <- dataset_bootstrap[dataset_bootstrap$Period_var_temp==period_list[a],]
        n[a] <- nrow(test1)
        n.BS[a] <- nrow(test2)
      }
      if(sum(n == n.BS)!=number_of_periods) print("error: the number is not equal in all periods")

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

      # Calculate HMTS
      HMTS_bootstrap <- calculate_HMTS_index(
        dataset = dataset_bootstrap
        , period_variable = 'Period_var_temp'
        , dependent_variable = dependent_variable
        , continious_variables = continious_variables
        , categorical_variables = categorical_variables
        , log_dependent = log_dependent
        , reference_period = reference_period
        , state_space_model = state_space_model
        , periods_in_year = periods_in_year
        , production_since = production_since
        , number_preliminary_periods = number_preliminary_periods
        , number_of_observations = FALSE
        , resting_points = FALSE
        , bootstrap = 5)   # bootstrap = Par_bootstrap

      # Calculate first and second moment: (EX)^2 and EX^2
      if(h == 1)  Sum.square <- HMTS_bootstrap$Index^2
      if(h > 1) Sum.square <- Sum.square + (HMTS_bootstrap$Index)^2

      if(h == 1)  Sum <- HMTS_bootstrap$Index
      if(h > 1) Sum <- Sum + HMTS_bootstrap$Index

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
    Lower_bound <- Results$Index - 1.96 * sqrt(variance)
    Upper_bound <- Results$Index + 1.96 * sqrt(variance)

    Results$Lower_bound <- Lower_bound
    Results$Upper_bound <- Upper_bound

    print(paste0("Bootstrap is done! Duration: "
                 , round(difftime(as.POSIXct(Sys.time()), starting_time,units="mins"),0)
                 ," min. "))

  }

  # Add resting points to output

  Imputations <- NULL # deze regel is als test toegevoed omdat ie nergens geinitialiseerd is en dus later verwijderen of anders definieren

  if(resting_points == TRUE){
    if(is.null(state_space_model) == FALSE){
      Results <- list(Index = Results
                      , Window = window
                      , Chosen_index_series = Imputations
                      , Matrix_HMTS_index = Tbl_resting_points$Matrix_HMTS_index
                      , Matrix_HMTS = Tbl_resting_points$Matrix_HMTS
                      , Matrix_HMTS_index = Tbl_resting_points$Matrix_HMTS_index
                      , Matrix_HMTS = Tbl_resting_points$Matrix_HMTS
                      , Matrix_HMTS_analyse = Tbl_resting_points$Matrix_HMTS_analyse)
    } else {
      Results <- list(Index = Results
                      , Window = window
                      , Chosen_index_series = Tbl_resting_points$Chosen_index_series
                      , Matrix_HMTS_index = Tbl_resting_points$Matrix_HMTS_index
                      , Matrix_HMTS = Tbl_resting_points$Matrix_HMTS)
    }
  }

  # Turn warnings back on
  # options(warn = defaultW)
  options(warn = 1)

  return(Results)

}

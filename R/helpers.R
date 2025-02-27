# In this file all invisible helper functions are placed




# First define the internal functions:

# the function 'Calculate_laspeyres' calls the folowing internal functions a) calculate_hedonic_imputation b) calculate_index
# c) show_progress_loop






### define the 1st internal function 'calculate_hedonic_imputation'


#' Title Calculation of a hedonic imputation
#' @author Farley Ishaak
#' @param dataset_temp = table with data (hoeft no selectie te zijn van benodigde variables)
#' @param Period_temp = 'Period'
#' @param dependent_variable_temp = usually the sale price
#' @param independent_variables_temp = vector with quality determining variables
#' @param log_dependent_temp = should the dependent variable be transformed to its logarithm? default = TRUE
#' @param period_list_temp = list with all available periods
#' @return
#'         table with imputation averages per period
#' @keywords internal


calculate_hedonic_imputation <- function(dataset_temp
                                         , Period_temp
                                         , dependent_variable_temp
                                         , independent_variables_temp
                                         , log_dependent_temp
                                         , number_of_observations_temp
                                         , period_list_temp){

  ## Data operations

  # Count number of periods
  number_of_periods <- length(period_list_temp)

  # Select required variables
  dataset_temp <- dataset_temp[, (names(dataset_temp) %in% c(Period_temp, dependent_variable_temp, independent_variables_temp))]

  # Remove lines without values
  dataset_temp[dataset_temp == ''] <- NA
  dataset_temp <- stats::na.omit(dataset_temp)

  # Remove unused levels. R remembers the original state of the levels, but if a level is not present in a certain period, this may result in an error in the bootstrap.
  dataset_temp <- droplevels(dataset_temp)

  # If parameter log_dependent = TRUE, transform to log
  if(log_dependent_temp == TRUE){
    dependent_variable_temp <- paste0("log(", dependent_variable_temp, ")")
  }

  ## Model

  # Compile model
  for (v in 1: length(independent_variables_temp)){
    if (v==1) {
      model <- paste0(dependent_variable_temp,"~",independent_variables_temp[v])
    } else {
      model <- paste0(model,"+",independent_variables_temp[v])
    }
  }

  ## Calculate imputations per period

  # Empty vector for the values and numbers
  average_Imputations <- c()
  number_observations_total <- c()

  Period_var_temp <- NULL # To prevent that the following note occurs: 'no visible binding for global variable 'Period_var_temp'
  for (p in 1:number_of_periods){

    # Estimate coefficients of the 1th period
    if (p==1) {
      Rekenbestand <- subset(dataset_temp, Period_var_temp == period_list_temp[1])
      fitmdl <- stats::lm(model,Rekenbestand)
      predictmdl_0 <- mean(stats::predict(fitmdl, Rekenbestand))

      # If parameter log_dependent = TRUE, then reverse with exp()
      if(log_dependent_temp == TRUE){predictmdl_0 <- exp(predictmdl_0)}

      # If parameter number_of_observations = TRUE, then calculate numbers
      if(number_of_observations_temp == TRUE){number <- nrow(Rekenbestand)}

    } else {
      # Estimate coefficients of all periods after
      Rekenbestand_t <- subset(dataset_temp,Period_var_temp==period_list_temp[p])
      fitmdl <- stats::lm(model,Rekenbestand_t)

      # If parameter number_of_observations = TRUE, then calculate numbers
      if(number_of_observations_temp == TRUE){number <- nrow(Rekenbestand_t)}

    }

    # Recoding of values, where the categorical variable has a level that is not estimated in the reference period
    Rekenbestand_0 <- Rekenbestand
    for (var in names(fitmdl$xlevels)) {
      missend_in_model <- levels(Rekenbestand_0[[var]])[!(levels(Rekenbestand_0[[var]]) %in% fitmdl$xlevels[[var]])]

      # Replace level of the variable by the first level (default). The variable is in fact not taken into account by this step in the calculation.
      sel <- Rekenbestand_0[[var]] %in% missend_in_model
      Rekenbestand_0[[var]][sel] <- fitmdl$xlevels[[var]][1]
    }
    predictmdl_t <- mean(predict(fitmdl, Rekenbestand_0))

    # If parameter log_dependent = TRUE, then reverse with exp()
    if(log_dependent_temp == TRUE){predictmdl_t <- exp(predictmdl_t)}

    average_Imputations[p] <- predictmdl_t

    # If parameter number_of_observations = TRUE, then add numbers to table
    if(number_of_observations_temp == TRUE){number_observations_total[p] <- number}

  }

  # Create table
  Tbl_average_imputation <- data.frame(Period=period_list_temp)

  # If parameter number_of_observations = TRUE, then add numbers to table
  if(number_of_observations_temp == TRUE){Tbl_average_imputation$number_of_observations <- number_observations_total}

  # Add imputations to table
  Tbl_average_imputation$average_imputation <- average_Imputations

  # Result
  return(Tbl_average_imputation)

}

### define the 2nd internal function 'calculate_index'

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
#' @keywords internal
calculate_index <- function(periods
                            , values
                            , reference_period = NULL
                            , from_growth_rate = FALSE){

  # Check length periods and values
  if(length(periods) != length(values)){
    stop('De reeks with periods is niet even lang als the reeks with values')
  }

  # Check numeric values
  if(is.numeric(values) == FALSE){
    stop('De reeks with values is niet (volledig) numeric')
  }

  # Transforms periods to characters
  periods <- as.character(periods)

  # If reference_period is not provided, then reference_period = 1th period from list
  if(is.null(reference_period) == TRUE){
    reference_period <- periods[1]
    periods_short <- periods
  } else {
    # Determine lengte reference_period
    Length_reference_period <- nchar(reference_period)
    periods_short <- substr(periods, 1, Length_reference_period)
  }

  # Check reference_period
  if(!reference_period %in% periods_short){
    stop('The provided reference period is not part of the series with periods')
  }

  # Transform growth_rate to index
  if(from_growth_rate == TRUE){
    values[1] <- 1
    values <- cumprod(values) * 100
  }

  # Create table
  Value <- NULL # To prevent that the following note occurs: 'no visible binding for global variable 'Value' '
  period <- NULL # To prevent that the following note occurs: 'no visible binding for global variable 'period' '
  Tbl_index <- data.frame(period = periods_short, Value = values)

  # Determine value reference_period
  Average <- dplyr::filter(Tbl_index, period == reference_period)
  Average <- dplyr::summarise(Average, Value = mean(Value))
  Average <- as.vector(Average$Value)

  # Calculate index
  Tbl_index$Index <- Tbl_index$Value/Average*100

  # Result = index series
  return(index = Tbl_index$Index)

}


### define the 3rd internal function 'show_progress_loop'

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


### define the 4th internal function "smooth.TS" to be used for the HMTS index


#' Run forward and backward pass of time series estimation
#'
#' Calculate a trend line based on a provided model.
#' This function is used in the function: calculate_trend_line_KFAS()
#'
#' @author Pim Ouwehand, Farley Ishaak
#' @param fittedmodel = model values as output of the function estimate.TS.parameters()
#' @return sub-list $signalsubconf[,1] provides the estimated trend line
#' @keywords internal
#' @import KFAS

smooth.TS <- function(fittedmodel){

  out_KFS    <- KFAS::KFS(fittedmodel, filtering='state', smoothing='state')
  signal3 <- KFAS::signal(out_KFS, states="all")
  signal <- signal3$signal
  signalvar <- signal3$variance[1,1,]
  signalsubset <- KFAS::signal(out_KFS, states=c("level", "regression"))
  signalsub <- signalsubset$signal
  signalsubvar <- signalsubset$variance[1,1,]

  state <- out_KFS$alphahat
  state[1,]
  statevar <- sapply(1:dim(out_KFS$V)[1], function(i) out_KFS$V[i,i,])
  StateSE <- sqrt(statevar) #defaultfouten
  state / StateSE #t-values
  stvar <- matrix(statevar, nrow=dim(statevar)[1], dimnames=list(c(), dimnames(state)[[2]]))
  mStSmo <- cbind(state, signal, stvar, signalvar)
  mStSmo

  state.est.1 <- t(rbind(state[1,], StateSE[1,], state[1,]/StateSE[1,]))
  dimnames(state.est.1)[[2]] <- c("Estimate", "Std.Error", "t-value") #column names

  # Store level and slope of 1th period in table
  Analyse_TS <- data.frame(Reeks = character(), Value = double())
  Analyse_TS[1, ] <- c('Level estimate (first period)', round(state.est.1[1,1], 4))
  Analyse_TS[2, ] <- c('Level std.error (first period)', round(state.est.1[1,2], 4))
  Analyse_TS[3, ] <- c('Level t-value (first period)', round(state.est.1[1,3], 4))
  Analyse_TS[4, ] <- c('Slope estimate (first period)', round(state.est.1[2,1], 4))
  Analyse_TS[5, ] <- c('Slope std.error (first period)', round(state.est.1[2,2], 4))
  Analyse_TS[6, ] <- c('Slope t-value (first period)', round(state.est.1[2,3], 4))

  last <- dim(state)[1]
  state.est.n <- t(rbind(state[last,], StateSE[last,], state[last,]/StateSE[last,]))
  dimnames(state.est.n)[[2]] <- c("Estimate", "Std.Error", "t-value") #column names

  # Store level and slopt of lasth period in table
  Analyse_TS[7, ] <- c('Level estimate (last period)', round(state.est.n[1,1], 4))
  Analyse_TS[8, ] <- c('Level std.error (last period)', round(state.est.n[1,2], 4))
  Analyse_TS[9, ] <- c('Level t-value (last period)', round(state.est.n[1,3], 4))
  Analyse_TS[10, ] <- c('Slope estimate (last period)', round(state.est.n[2,1], 4))
  Analyse_TS[11, ] <- c('Slope std.error (last period)', round(state.est.n[2,2], 4))
  Analyse_TS[12, ] <- c('Slope t-value (last period)', round(state.est.n[2,3], 4))

  conf_series1 <- predict(fittedmodel, interval = "confidence", level = 0.95)
  LB <- signal - qnorm(0.975)*sqrt(signalvar)
  UB <- signal + qnorm(0.975)*sqrt(signalvar)
  signalconf <- cbind(signal, LB, UB)
  signalconf - conf_series1

  LBtrend <- signalsub - qnorm(0.975)*sqrt(signalsubvar)
  UBtrend <- signalsub + qnorm(0.975)*sqrt(signalsubvar)
  signalsubconf <- cbind(trend=signalsub, LB=LBtrend, UB=UBtrend)

  return(list(out_KFS=out_KFS, signal=signal, signalvar=signalvar, signalconf=signalconf,
              signalsub=signalsub, signalsubvar=signalsubvar, signalsubconf=signalsubconf,
              mStSmo=mStSmo, analyse_TS = Analyse_TS))

}

### define the 5th internal function "estimate.TS.parameters" to be used for the HMTS index

#' Estimate time series parameters
#'
#' Estimate parameters to estimate trend lines
#' This function is used in the function: calculate_trend_line_KFAS()#'
#'
#' @author Pim Ouwehand, Farley Ishaak
#' @param model = model values as output of the function select_state_space_model()
#' @param initial.values = $initial.values as output of the model
#' @return parameter for the time series model
#' @keywords internal

estimate.TS.parameters <- function(model, initial.values){

  # original series, same as series
  y <- model$y

  fitmdl <- KFAS::fitSSM(model=model, inits=initial.values, method='BFGS', hessian=TRUE)
  LogLikValue <- -fitmdl$optim.out$value # provides logLik
  LogLikValue2 <- -fitmdl$optim.out$value/length(y) # as in Commandeur and Koopman

  q <- sum(model$P1inf) # number of diffuse initial values in the state
  w <- sum(is.na(model$Q)) + sum(is.na(model$H)) # total number of disturbance variances estimated

  AIC <- (-2*LogLikValue+2*(q+w))
  AIC2 <- AIC/length(y)  # as in Commandeur and Koopman

  LikAIC <- matrix(c(LogLikValue, AIC, LogLikValue2, AIC2), nrow=2,
                   dimnames=list(c("LogLik", "AIC"), c("value", "value/n")))

  # Store estimates in table
  Analyse_TS <- data.frame(Reeks = character(), Value = double())
  Analyse_TS[1, ] <- c('Log likelihood model', round(LogLikValue, 3))
  Analyse_TS[2, ] <- c('Log likelihood model /n', round(LogLikValue2, 3))
  Analyse_TS[3, ] <- c('AIC', round(AIC, 3))
  Analyse_TS[4, ] <- c('AIC /n', round(AIC2, 3))

  # Store estimates in table
  Analyse_TS[5, ] <- c('q (#diffuse states)', q)
  Analyse_TS[6, ] <- c('w (#hyp.par)', w)

  sigma.sq <- exp(fitmdl$optim.out$par)  # maximum likelihood estimate of the stdev of the disturbances
  sigma    <- sqrt(sigma.sq)

  se <- fitmdl$optim.out$par*NA
  try({ H <- fitmdl$optim.out$hessian
  cov <- solve(-H)
  se  <- sqrt(diag(-cov)) #standard errors
  })

  tvalues <- abs( fitmdl$optim.out$par / se )

  volgorde <- c(length(sigma), 1:(length(sigma)-1))
  if(length(sigma)>1) volgorde <- c(length(sigma), 1:(length(sigma)-1))
  if(length(sigma)<=1) volgorde <- length(sigma)
  sigma2 <- sigma[volgorde]
  sigma.sq2 <- sigma.sq[volgorde]
  tvalues2 <- tvalues[volgorde]

  par.est <- t(rbind(sigma2, sigma.sq2, tvalues2))  #
  dimnames(par.est)[[2]] <- c("sigma", "sigma^2", "t-value") #column names

  # Store estimates in table
  Analyse_TS[7, ] <- c('meas sigma', round(par.est[1,1], 3))
  Analyse_TS[8, ] <- c('meas sigma^2', round(par.est[1,2], 4))
  Analyse_TS[9, ] <- c('meas t-value', round(par.est[1,3], 3))
  Analyse_TS[10, ] <- c('slope sigma', round(par.est[2,1], 3))
  Analyse_TS[11, ] <- c('slope sigma^2', round(par.est[2,2], 4))
  Analyse_TS[12, ] <- c('slope t-value', round(par.est[2,3], 3))

  return(list(fitmdl=fitmdl, LikAIC=LikAIC, sigma=sigma, analyse_TS = Analyse_TS))

} # end function

### define the 6th internal function "determine_initial_parameters" to be used for the HMTS index

#' Determine_initial_parameters
#'
#' Determine startvalues within state space models
#' This function is used in the function: calculate_trend_line_KFAS()
#'
#' @author Pim Ouwehand, Farley Ishaak
#' @param model = modelvalues as output of the function select_state_space_model()
#' @param initial.values = $initial.values as output of the model
#' @param FUN = function called: defaultupdatefn
#' @return New initial startvalues
#' @keywords internal

determine_initial_parameters <- function (model, initial.values, FUN=defaultupdatefn){

  # update the Q-matrix (only once)
  model2 <- FUN(initial.values, model)

  # Run the Kalman Filter in order to compute the LogLikelihood:
  KF <- KFS(model2, filtering = "state", smoothing = "state")

  # Create table for analysis
  Analyse_TS <- data.frame(Reeks = character(), Value = double())

  #1
  Analyse_TS[1, ] <- c('Log likelihood startvalues', round(KF$logLik,3))

  # in SsfPack the scale factor is returned from function 'SsfLikEx'
  # Here, we have to compute it manually:
  d = sum(model2$P1inf) #number of diffuse initial states
  n <- length(model2$y) # number of observations

  v = KF$v[(d+1):n]
  F1 = KF$F[(d+1):n]^-1
  vF1v <- v*F1*v  #v'_t * F_t^-1 * v_t

  # loglik according to KFAS Vignette section 2.1
  # univariate treatment and diffuse initialization, the diffuse log-likelihood is:
  LogLik.diffuse <- -0.5*sum(log(KF$Finf)) -0.5*sum(log(2*pi)+log(KF$F[(d+1):n])+ vF1v)
  # not needed here, but gives same result as KF$logLik
  # and confirms that vF1v is computed correctly, and is needed to compute the scale factor.
  difference.logLik = round(KF$logLik,2) - round(LogLik.diffuse,2)
  if(difference.logLik != 0){
    print(paste0('difference in LogLiks:',   KF$logLik, ' vs. ', LogLik.diffuse))
  }

  # scale factor (see equation 11.7 in Commandeur and Koopman or 9.4 in Ssfpack manual, Koopman, Shepherd, Doornik, 2008)
  scale <- 1/(n-d)*sum(vF1v)

  #2
  Analyse_TS[2, ] <- c('Scale factor', round(scale,5))

  #use scale factor to update initial values of hyperparameters:
  initial.values2 <- initial.values + 0.5*log(scale)

  #3
  Analyse_TS[3, ] <- c('Slope', initial.values2[1])
  Analyse_TS[4, ] <- c('Meas', initial.values2[2])

  return(list(initial.values2 = initial.values2, analyse_TS = Analyse_TS))
}


### define the 7th internal function "select_state_space_model" to be used for the HMTS index

#' Select the state space model type
#'
#' This function is used in the function: calculate_trend_line_KFAS()
#'
#' @author Pim Ouwehand
#' @param selection = the model number for the trend calculation (Default = 5).
#' @param series = time series with values in chronological order
#' @param regressors = optional external variable which influences the series (defaul = NULL)
#' @param initial.values.all = =  startvalues for 5 hyperparameters: meas, level, slope, seas, scaling
#' @return modelvalues (level, slope) of the chosen state space model and the provided time series
#' @keywords internal

select_state_space_model <- function(selection = 5, series, regressors=NULL, initial.values.all){

  periodicity <- stats::tsp(series)[3]

  if(selection==1){
    modelname <- "det. level"

    model <- KFAS::SSModel(series ~ SSMtrend(1,Q=0),H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series ~ SSMtrend(1,Q=0) + regressors,H=matrix(NA))
    }
    initial.values <- initial.values.all[1]
  }

  if(selection==2){
    modelname <- "stoch.level"

    model <- KFAS::SSModel(series ~ SSMtrend(1,Q=list(matrix(NA))),H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series ~ SSMtrend(1,Q=list(matrix(NA))) + regressors,H=matrix(NA))
    }
    initial.values <- initial.values.all[c(2,1)]
  }

  if(selection==3){
    modelname <- "det. level + det. slope (= classical regression)"
    model <- KFAS::SSModel(series~SSMtrend(2,Q=list(0, 0)),H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~SSMtrend(2,Q=list(0, 0)) + regressors,H=matrix(NA))
    }
    initial.values <- initial.values.all[1]

  }

  if(selection==4){
    modelname <- "stoch.level + det. slope"
    model <- KFAS::SSModel(series~SSMtrend(2,Q=list(matrix(NA), 0)) ,H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~SSMtrend(2,Q=list(matrix(NA), 0)) + regressors,H=matrix(NA))
    }
    initial.values <- initial.values.all[c(2,1)]
  }

  if(selection==5){
    modelname <- "det. level + stoch.slope (=smooth trend)"

    model <- KFAS::SSModel(series~SSMtrend(2,Q=list(0, matrix(NA))) ,H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~SSMtrend(2,Q=list(0, matrix(NA))) + regressors,H=matrix(NA))
    }
    initial.values <- initial.values.all[c(3,1)]
  }

  if(selection==6){
    modelname <- "stoch.level + stoch.slope"

    model <- KFAS::SSModel(series~SSMtrend(2,Q=list(matrix(NA), matrix(NA))),H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~SSMtrend(2,Q=list(matrix(NA), matrix(NA))) + regressors,H=matrix(NA))
    }
    initial.values <- initial.values.all[c(2,3,1)]
  }

  if(selection==7){
    modelname <- "det. level +               det. seasonal"

    model <- KFAS::SSModel(series~ -1 + SSMtrend(1,Q=0)
                           + SSMseasonal(periodicity, Q=0, sea.type="dummy")
                           ,H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~ -1 + SSMtrend(1,Q=0)
                             + SSMseasonal(periodicity, Q=0, sea.type="dummy")
                             + regressors ,H=matrix(NA))
    }
    initial.values <- initial.values.all[1]
  }

  if(selection==8){
    modelname <- "stoch.level +               det. seasonal"

    model <- KFAS::SSModel(series~ -1 + SSMtrend(1,Q=list(matrix(NA)))
                           +SSMseasonal(periodicity, diag(0,1), sea.type="dummy")
                           ,H=matrix(NA)  )
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~ -1 + SSMtrend(1,Q=list(matrix(NA)))
                             +SSMseasonal(periodicity, diag(0,1), sea.type="dummy")
                             + regressors ,H=matrix(NA)  )
    }
    initial.values <- initial.values.all[c(2,1)]
  }

  if(selection==9){
    modelname <- "stoch.level + det. slope + det. seasonal"

    model <- KFAS::SSModel(series~SSMtrend(2,Q=list(matrix(NA), 0))
                           +SSMseasonal(periodicity, diag(0,1), sea.type="dummy")
                           ,H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~SSMtrend(2,Q=list(matrix(NA), 0))
                             + SSMseasonal(periodicity, diag(0,1), sea.type="dummy")
                             + regressors,H=matrix(NA))
    }
    initial.values <- initial.values.all[c(2,1)]
  }

  if(selection==10){
    modelname <- "det. level + stoch.slope + det. seasonal (smooth trend + seas)"

    model <- KFAS::SSModel(series~SSMtrend(2,Q=list(0, matrix(NA)))
                           +SSMseasonal(periodicity, diag(0,1), sea.type="dummy")
                           ,H=matrix(NA))
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~SSMtrend(2,Q=list(0, matrix(NA)))
                             + SSMseasonal(periodicity, diag(0,1), sea.type="dummy")
                             + regressors,H=matrix(NA))
    }
    initial.values <- initial.values.all[c(3,1)]
  }

  if(selection==11){
    modelname <- "stoch.level + stoch.slope + det. seasonal"

    model <- KFAS::SSModel(series~-1 +SSMtrend(2,Q=list(matrix(NA), matrix(NA)))
                           + SSMseasonal(periodicity, diag(0,1), sea.type="dummy")
                           ,H=matrix(NA)  )
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~-1 +SSMtrend(2,Q=list(matrix(NA), matrix(NA)))
                             + SSMseasonal(periodicity, diag(0,1), sea.type="dummy")
                             + regressors,H=matrix(NA)  )
    }
    initial.values <- initial.values.all[c(2,3,1)]
  }

  ###########################
  # 12. Local level + (stoch.) seasonal model
  ###########################
  # y_t   = mu_t + gamma_t + epsilon_t
  # mu_t+1= mu_t + eta_t
  # gamma_t+1 = -gamma_t - ... + eta3_t
  ###########################
  if(selection==12){
    modelname <- "stoch.level +               stoch.seasonal"

    model <- KFAS::SSModel(series ~ -1 + SSMtrend(1,Q=list(matrix(NA)))
                           + SSMseasonal(periodicity, diag(NA,1), sea.type="dummy")
                           ,H=matrix(NA)  )
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series ~ -1 + SSMtrend(1,Q=list(matrix(NA)))
                             + SSMseasonal(periodicity, diag(NA,1), sea.type="dummy")
                             + regressors ,H=matrix(NA)  )
    }
    initial.values <- initial.values.all[c(2,4,1)]
  }

  ########################################
  # 13. Local level, det. slope + stoch. seasonal
  #########################################
  # y_t   = mu_t + gamma_t + epsilon_t
  # mu_t+1= mu_t + v_t + eta1_t
  # v_t+1 = v_t
  # gamma_t+1 = -gamma_t - ... + eta3_t
  ###########################
  if(selection==13){
    modelname <- "stoch.level + det. slope + stoch.seasonal"

    model <- KFAS::SSModel(series~-1 +SSMtrend(2,Q=list(matrix(NA),0))
                           + SSMseasonal(periodicity, diag(NA,1), sea.type="dummy")
                           ,H=matrix(NA)  )
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~-1 +SSMtrend(2,Q=list(matrix(NA),0))
                             + SSMseasonal(periodicity, diag(NA,1), sea.type="dummy")
                             + regressors,H=matrix(NA)  )
    }
    initial.values <- initial.values.all[c(2,4,1)]

  }

  ########################################
  # 14. Smooth trend  + stoch. seasonal model
  #########################################
  # y_t   = mu_t + gamma_t + epsilon_t
  # mu_t+1= mu_t + v_t
  # v_t+1 = v_t  + eta2_t
  # gamma_t+1 = -gamma_t - ... + eta3_t
  ###########################
  if(selection==14){
    modelname <- "det.  level + stoch.slope + stoch.seasonal (smooth trend + seas)"
    model <- KFAS::SSModel(series~-1 +SSMtrend(2,Q=list(0, matrix(NA)))
                           + SSMseasonal(periodicity, diag(NA,1), sea.type="dummy")
                           ,H=matrix(NA)  )
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~-1 +SSMtrend(2,Q=list(0, matrix(NA)))
                             + SSMseasonal(periodicity, diag(NA,1), sea.type="dummy")
                             + regressors,H=matrix(NA)  )
    }
    initial.values <- initial.values.all[c(3,4,1)]

  }

  ###############################################
  # 15. Local linear trend + stoch. seasonal model
  ################################################
  # y_t   = mu_t + gamma_t + epsilon_t
  # mu_t+1= mu_t + v_t + eta1_t
  # v_t+1 = v_t  + eta2_t
  # gamma_t+1 = -gamma_t - ... + eta3_t
  ###########################
  if(selection==15){
    modelname <- "stoch.level + stoch.slope + stoch.seasonal"
    model <- KFAS::SSModel(series~-1 +SSMtrend(2,Q=list(matrix(NA), matrix(NA)))
                           + SSMseasonal(periodicity, diag(NA,1), sea.type="dummy")
                           ,H=matrix(NA)  )
    if(is.null(regressors)==F) {
      model <- KFAS::SSModel(series~-1 +SSMtrend(2,Q=list(matrix(NA), matrix(NA)))
                             + SSMseasonal(periodicity, diag(NA,1), sea.type="dummy")
                             + regressors,H=matrix(NA)  )
    }
    initial.values <- initial.values.all[c(2,3,4,1)]
  }

  num.models <- 15

  return(list(modelname=modelname, model=model, initial.values=initial.values, num.models=num.models))

} #end function



### define the 8th internal function "calculate_trend_line_KFAS" to be used for the HMTS index

#' Calculate the trend line for a provided time series of numeric values
#'
#' Calculate the trend line with the state space method for a provided time series (chronological order is assumed).
#' The series are calculated with the package KFAS.
#'
#' @author Pim Ouwehand, Farley Ishaak
#' @param original_series = time series with values in chrolological order
#' @param periodicity = if month, then  12. If quarter, then 4, etc. (defaul = 4)
#' @param modelnumber = the modelnumber for the trend calcualtion (default = 5 -> smooth trend) No trend = NULL
#' @param logarithmic = logarithm of the dependent variable? (default = TRUE)
#' @param regressor = optional external variable that influences the series (default = NULL)
#' @param startvalues =  startvalues for 5 hyperparameters: meas, level, slope, seas, scaling. default = NULL: optimal estimation of these values.
#' @param resting_points = Should analyses values be returned? (default = FALSE)
#' @return Trend line
# @examples
#series <- c(85, 97, 100, 104, 111)
#calculate_trend_line_KFAS(series)

calculate_trend_line_KFAS <- function(original_series
                                      , periodicity = 4
                                      , modelnumber = 5
                                      , logarithmic = TRUE
                                      , regressor = NULL
                                      , startvalues = NULL
                                      , resting_points = FALSE){

  # Transform with logarithm
  if(logarithmic == TRUE){
    original_series <- log(original_series)
  }

  # Transform in time series
  Origineel_TS <- stats::ts(original_series, start = 1, frequency = periodicity, names="Origineel_TS")

  # Startvalues
  if(is.null(startvalues) == TRUE){
    startvalues_old <- set_startvalues(log(0.1), log(0.1), log(0.1),log(0.1),log(0.1))

    # Analyses
    modelsel <- select_state_space_model(selection = modelnumber
                                         , series = Origineel_TS
                                         , regressors = regressor
                                         , initial.values.all = startvalues_old)

    model_SS <- model <- modelsel$model
    startvalues_temp <- modelsel$initial.values
    startvalues_temp <- determine_initial_parameters(model, initial.values = startvalues_temp)
    startvalues_new <- startvalues_temp$initial.values2
    startvalues_analyse <- startvalues_temp$analyse_TS


  } else {

    # Select model
    model <- select_state_space_model(selection = modelnumber
                                      , series = Origineel_TS
                                      , regressors = regressor
                                      , initial.values.all = startvalues)

    startvalues_new <- startvalues

    # Determine trend line
    model_SS <- model$model
  }


  ML <- estimate.TS.parameters(model = model_SS, initial.values = startvalues_new)
  parameters_analyse <- ML$analyse_TS
  SM <- smooth.TS(ML$fitmdl$model)
  model_analyse <- SM$analyse_TS

  # Transform trend line back if logarithmic = TRUE
  if(logarithmic == TRUE){
    trend_line <- exp(SM$signalsubconf[,1])
  } else {
    trend_line <- SM$signalsubconf[,1]
  }

  # Addd resting_points
  if(resting_points == TRUE){
    if(is.null(startvalues) == TRUE){
      Analyses_complete <- rbind(startvalues_analyse, parameters_analyse, model_analyse)
    } else {
      Analyses_complete <- rbind(parameters_analyse, model_analyse)
    }
    trend_line <- list(trend_line = trend_line, resting_points = Analyses_complete)
  }

  return(trend_line)
}



### define the 9th internal function "defaultupdatefn" to be used for the HMTS index

#' Default update function
#'
#' This function is used in the function: calculate_trend_line_KFAS()
#'
#' @author Pim Ouwehand
#' @param pars = startvalues
#' @param model = state space modelnumber
#' @return newmodel
#' @keywords internal

defaultupdatefn <- function(pars, model){

  # default updatefunction, copied from help files
  # see KFAS Help function for 'fitSSM' :  help(fitSSM)

  if(any(is.na(model$Q))){
    Q <- as.matrix(model$Q[,,1])
    naQd  <- which(is.na(diag(Q)))
    naQnd <- which(upper.tri(Q[naQd,naQd]) & is.na(Q[naQd,naQd]))
    Q[naQd,naQd][lower.tri(Q[naQd,naQd])] <- 0
    diag(Q)[naQd] <- exp(0.5 * pars[1:length(naQd)])
    Q[naQd,naQd][naQnd] <- pars[length(naQd)+1:length(naQnd)]
    model$Q[naQd,naQd,1] <- crossprod(Q[naQd,naQd])
  }
  if(!identical(model$H,'Omitted') && any(is.na(model$H))){#'
    H<-as.matrix(model$H[,,1])
    naHd  <- which(is.na(diag(H)))
    naHnd <- which(upper.tri(H[naHd,naHd]) & is.na(H[naHd,naHd]))
    H[naHd,naHd][lower.tri(H[naHd,naHd])] <- 0
    diag(H)[naHd] <-
      exp(0.5 * pars[length(naQd)+length(naQnd)+1:length(naHd)])
    H[naHd,naHd][naHnd] <-
      pars[length(naQd)+length(naQnd)+length(naHd)+1:length(naHnd)]
    model$H[naHd,naHd,1] <- crossprod(H[naHd,naHd])
  }
  model
}


### define the 10th internal function 'set_startvalues' to be used for the HMTS index



# Set starting values for hyperparameters in state space models

#' Title
#'
#' @param a  mean
#' @param b  level
#' @param c  slope
#' @param d  seas
#' @param e  scaling
#'
#' @return starting values for hyperparameters
#'
#' @keywords internal
set_startvalues <- function(a,b,c,d,e){

  init <- c(a,b,c,d,e)
  names(init) <- c("meas", "level", "slope", "seas", "scaling")

  return(init = init)

}



### define the 11th internal function 'calculate_hedonic_imputationmatrix' to be used for the HMTS index

#' Calculate a matrix with hedonic imputation averages, re-estimated time series imputation averages and  corresponding index series.
#'
#' Based on a hedonic model, a series of imputated values is calculated in below steps:
#' 1: for every period average imputed prices are estimated with the 1th period as base period.
#' 2: the above is repeated for each possible base period. This result in an equal number of series as the number of periods.
#' 3: All series are re-estimated with a time series model (state space).
#'    This step is optionally skipped with a parameter (state_space_model = NULL)
#' 4: the series imputed values are transformed into index series.
#' This matrix can be used for an index calculations according to the HMTS method.
#'
#' Parameter 'production_since':
#' To simulate a series, where 1 period a time expires (as in production), a manual choice in the past is possible.
#' Untill this period, all periods are imputed. After that, 1 period is added.
#'
#' @author Farley Ishaak
#' @param period_variable = variable in the dataset with the period
#' @param dependent_variable = usually the sale price
#' @param continious_variables = vector with quality-determining continues variables (numeric, no dummies)
#' @param categorical_variables = vector with categorical variables (also dummy)
#' @param log_dependent = should the dependent variable be transformed to its logarithm? default = TRUE
#' @param state_space_model = the model number of the trend calculation (default = 5 -> smooth). No trend = NULL
#' @param number_of_observations = number of observations per period (default = TRUE)
#' @param periods_in_year = if month, then 12. If quarter, then 4, etc. (default = 4)
#' @param production_since = 1 period in the format of the period_variable. See description above (default = NULL)
#' @param number_preliminary_periods = number of periods that the index is preliminary. Only works if production_since <> NULL. default = 3
#' @param bootstrap = the number of iterations for calculating a confidence interval (usually 500) (default = NULL -> no intervals)
#' @return
#' $Matrix_HMTS_index = table with index series based on estimations with time series re-estimations
#' $Matrix_HMTS = table with estimated values based on time series re-estimations
#' $Matrix_HMS_index = table with index series based on estimations with the hedonic model
#' $Matrix_HMS = table with estimated values based on the hedonic model
#' $Matrix_HMTS_analysis = table with analysis values of the time series model per base period
#' @keywords internal


calculate_hedonic_imputationmatrix <- function(dataset
                                               , period_variable
                                               , dependent_variable
                                               , continious_variables
                                               , categorical_variables
                                               , log_dependent = FALSE
                                               , state_space_model = 5
                                               , periods_in_year = 4
                                               , number_of_observations = FALSE
                                               , production_since = 201404
                                               , number_preliminary_periods = 3
                                               , bootstrap = NULL){

  ## Data operations

  # Merge independent variables
  independent_variables <- c(continious_variables, categorical_variables)

  # Rename period_variable and transform to character

  Period_var_temp <- NULL # To prevent that the following note occurs: 'no visible binding for global variable 'Period_var_temp'
  dataset <- dplyr::rename(dataset, Period_var_temp = period_variable)
  dataset$Period_var_temp <- as.character(dataset$Period_var_temp)

  # Create period_list
  period_list <- sort(unique(dataset$Period_var_temp))

  # Count number of periods
  number_periods <- length(period_list)

  # Find period for production_since
  if(is.null(production_since) == FALSE){
    production_since_index <- match(production_since, period_list)
    if(is.na(production_since_index) == TRUE){stop('The provided production_since-period is not part of the periods in the data. Check the notation.')}
  } else {
    production_since_index <- number_preliminary_periods <- number_periods}

  # Select necessary variables
  dataset_temp <- dataset[, (names(dataset) %in% c('Period_var_temp', dependent_variable, independent_variables))]

  # Remove lines without values
  dataset_temp[dataset_temp == ''] <- NA
  dataset_temp <- na.omit(dataset_temp)

  # Remove unused levels. R remembers the original state of the levels, but if a level is not present in a certain period, this may result in an error in the bootstrap.
  dataset_temp <- droplevels(dataset_temp)

  # If parameter log_dependent = TRUE, transform to log
  if(log_dependent == TRUE){
    dependent_variable <- paste0("log(", dependent_variable, ")")
  }

  ## Model

  # Compile model
  for (v in 1: length(independent_variables)){
    if (v==1) {
      model <- paste0(dependent_variable,"~",independent_variables[v])
    } else {
      model <- paste0(model,"+",independent_variables[v])
    }
  }

  ## Calculate imputations per period

  # Empty vector for the numbers
  number_observations_total <- c()

  # Create tables with periods (imputations are added later on)
  Matrix_HMS <- Matrix_HMS_index <- Matrix_HMTS <- Matrix_HMTS_index<- data.frame(Period=period_list)

  for (p in 1:number_periods){

    # At the moment that the reporting period is preliminary, the reeks are allowed to expand
    if(p <= production_since_index - number_preliminary_periods){number_periods_production_since <- production_since_index}
    if(p > production_since_index - number_preliminary_periods){number_periods_production_since <- p+number_preliminary_periods}
    if(number_periods_production_since > number_periods){number_periods_production_since <- number_periods}

    # Calculate number of empty periods in series
    Difference_length_series <- number_periods - number_periods_production_since

    # Filter dataset on base period
    Dataset_base <- subset(dataset_temp, Period_var_temp == period_list[p])

    # Store number of observations
    if(number_of_observations == TRUE){number_observations_total[p] <- nrow(Dataset_base)}

    # Empty vectors for the values
    HMS <- c()
    HMS_index <- c()
    HMTS <- c()
    HMTS_index <- c()
    HMTS_analysis <- c()

    for (q in 1:number_periods_production_since){

      # Filter dataset on reporting period
      Dataset_dynamic <- subset(dataset_temp, Period_var_temp == period_list[q])
      fitmdl <- lm(model, Dataset_dynamic)

      # Recoding of values, where the categorical variable has a level that is not estimated in the reference period
      for (var in names(fitmdl$xlevels)) {
        missend_in_model <- levels(Dataset_base[[var]])[!(levels(Dataset_base[[var]]) %in% fitmdl$xlevels[[var]])]
        sel <- Dataset_base[[var]] %in% missend_in_model
        Dataset_base[[var]][sel] <- fitmdl$xlevels[[var]][1]
      }

      # Calculate imputation
      predictmdl <- mean(stats::predict(fitmdl, Dataset_base))

      # If parameter log_dependent = TRUE, then reverse with exp()
      if(log_dependent == TRUE){predictmdl <- exp(predictmdl)}

      # Place imputation in total vector
      HMS[q] <- predictmdl

    }

    ## Calculations without state space

    # Calculate index
    HMS_index <- calculate_index(periods = c(1:number_periods_production_since)
                                 , values = HMS
                                 , reference_period = NULL)

    ## Calculations with state space

    if(is.null(state_space_model) == FALSE){

      # Calculate trend line and growth_rate
      HMTS_temp <- calculate_trend_line_KFAS(original_series = HMS, modelnumber = state_space_model, periodicity = periods_in_year, resting_points = TRUE)
      HMTS <- HMTS_temp$trend_line
      HMTS_analysis <- HMTS_temp$resting_points
      HMTS_index <- calculate_index(periods = c(1:number_periods_production_since), values = HMTS)

      # Add NA for not calculated periods at the end of simulated series
      HMTS <- c(HMTS, rep(NA, Difference_length_series))
      HMTS_index <- c(HMTS_index, rep(NA, Difference_length_series))

      # Add series to matrices
      Matrix_HMTS[paste0("Base_", period_list[p])] <- HMTS
      Matrix_HMTS_index[paste0("Base_", period_list[p])] <- HMTS_index

      if(p == 1){
        Matrix_HMTS_analysis <- data.frame(Reeks = HMTS_analysis$Reeks)
      }
      Matrix_HMTS_analysis[paste0("Base_", period_list[p])] <- HMTS_analysis$Value

    }

    ## Calculations without state space

    # Add NA for not calculated periods at the end of simulated series
    HMS <- c(HMS, rep(NA, Difference_length_series))
    HMS_index <- c(HMS_index, rep(NA, Difference_length_series))

    # Add series to matrices
    Matrix_HMS[paste0("Base_", period_list[p])] <- HMS
    Matrix_HMS_index[paste0("Base_", period_list[p])] <- HMS_index

    # Show progress (if bootstrap is NULL. Otherwise, bootstrap progress has priority)
    if(is.null(bootstrap) == TRUE){show_progress_loop(p, number_periods)}

  }

  # Add numbers to calculation
  if(number_of_observations == TRUE){
    Matrix_HMS["Number_observations"] <- Matrix_HMS_index["Number_observations"] <- number_observations_total
    if(is.null(state_space_model) == FALSE){Matrix_HMTS["Number_observations"] <- Matrix_HMTS_index["Number_observations"] <- number_observations_total}
  }

  # Voeg matrices samen in lijst
  if(is.null(state_space_model) == FALSE){
    Matrices <- list(Matrix_HMS = Matrix_HMTS
                     , Matrix_HMS_index = Matrix_HMS_index
                     , Matrix_HMTS = Matrix_HMTS
                     , Matrix_HMTS_index= Matrix_HMTS_index
                     , Matrix_HMTS_analysis = Matrix_HMTS_analysis)
  } else {
    Matrices <- list(Matrix_HMS = Matrix_HMS, Matrix_HMS_index = Matrix_HMS_index)
  }

  return(Matrices)

}



### define the 13th internal function 'calculate_geometric_average' to be used for the function  'Calculate_HMTS_index'


#' Calculate the geometric average of a series of values
#'
#' The equation for the calculation is:: exp(mean(log(reeks_values)))
#'
#' @author Farley Ishaak (FIHK)
#' @param values = series with numeric values
#' @return geometric average

calculate_geometric_average <- function(values){

  # Remove NA values
  values <- values[!is.na(values)]

  # Check numeric values
  if(is.numeric(values) == FALSE){stop('De reeks with values is niet (volledig) numeric')}

  # Calculate geometric average
  exp(mean(log(values)))

}

### define the 13th internal function 'Calculate_HMTS_index' to be used for the HMTS index

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
#' @return
#' $Matrix_HMTS_index = table with index series based on estimations with time series re-estimations
#' $Matrix_HMTS = table with estimated values based on time series re-estimations
#' $Matrix_HMS_index = table with index series based on estimations with the hedonic model
#' $Matrix_HMS = table with estimated values based on the hedonic model
#' $Matrix_HMTS_analysis = table with analysis values of the time series model per base period
#' @keywords internal
#' @return table with periods, index and number of observations. If resting_points = TRUE, then list with tables. See general description and examples.


calculate_HMTS_index <- function(
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
    number_preliminary_periods = 3,
    number_of_observations = TRUE,
    resting_points = FALSE,
    bootstrap = NULL) {

  # Rename period_variable and transform to character
  dataset <- dplyr::rename(dataset, Period_var_temp = period_variable)
  dataset$Period_var_temp <- as.character(dataset$Period_var_temp)

  # Count number of periods
  period_list <- sort(unique(dataset$Period_var_temp))
  number_of_periods <- length(period_list)
  if(is.null(number_preliminary_periods)==TRUE){
    number_preliminary_periods <- number_of_periods
  }
  number_of_periods_min_Window <- length(period_list) - number_preliminary_periods

  # Calculate matrix with imputations (base period x reporting period)
  Imputations_complete <- calculate_hedonic_imputationmatrix(
    dataset = dataset
    , period_variable = 'Period_var_temp'
    , dependent_variable = dependent_variable
    , continious_variables = continious_variables
    , categorical_variables = categorical_variables
    , log_dependent = log_dependent
    , state_space_model = state_space_model
    , number_of_observations = number_of_observations
    , production_since = production_since
    , number_preliminary_periods = number_preliminary_periods
    , bootstrap = bootstrap)

  # Transform list to tables
  Matrix_HMTS <- as.data.frame(Imputations_complete$Matrix_HMTS)
  Matrix_HMTS_index <- as.data.frame(Imputations_complete$Matrix_HMTS_index)

  # Determine which matrix to use (state space re-estimations or not)
  if(is.null(state_space_model) == FALSE){
    Matrix_HMTS <- as.data.frame(Imputations_complete$Matrix_HMTS)
    Matrix_HMTS_index <- as.data.frame(Imputations_complete$Matrix_HMTS_index)
    Matrix_HMTS_analyse <- as.data.frame(Imputations_complete$Matrix_HMTS_analyse)
    Imputations <- Matrix_HMTS_index
  } else {
    Imputations <- Matrix_HMTS_index
  }

  # The values start in the second column (with periods)
  start_window <- 2

  ## Calculate window average per period
  if(number_preliminary_periods != number_of_periods){

    for (p in 1:number_of_periods){

      # Determine window (splice) 1th period
      if(p==1){
        window <- Imputations[p, c(p:(number_preliminary_periods+start_window))]
        window$Period <- NULL
        end_window <- start_window
        start_window_update <- start_window + 1
      }

      # Determine other periods within 1th window
      if(p>1 & p<=number_preliminary_periods + 1){
        end_window <- end_window + 1
        end_window <- number_preliminary_periods + start_window
        window <- dplyr::bind_rows(window, as.data.frame(Imputations[p, c(start_window:end_window)]))
      }

      # Determine window (splice) other periods
      if(p > number_preliminary_periods + 1){
        end_window <- end_window + 1
        if(number_preliminary_periods == 0){
          window_plus_1 <- as.data.frame(Imputations[p, c((start_window_update-1):end_window)])
          window_plus_1[,1] <- NA
          window <- dplyr::bind_rows(window, window_plus_1)
        } else {
          window <- dplyr::bind_rows(window, as.data.frame(Imputations[p, c(start_window_update:end_window)]))
        }
        start_window_update <- start_window_update + 1
      }

    }

  }

  # Transpose the table to prepare geometric average per column
  if(number_preliminary_periods == number_of_periods){
    window <- Imputations[, -1]
  }
  window_transposed <- as.data.frame(t(window))
  Geometric_averages <- c()

  # Calculate geometric average per column
  for (i in 1:number_of_periods) {

    # Calculate geometric average for each column
    Geometric_average <- calculate_geometric_average(window_transposed[, i][!is.na(window_transposed[, i])])

    # Add average to vector
    Geometric_averages[i] <- Geometric_average

  }

  # Add geometric averages to table for analyses
  window$Period <- period_list
  window <- window[, c(number_of_periods + 1, 1:number_of_periods)]
  Imputations$Geom_avg <- Matrix_HMTS$Geom_avg <- Matrix_HMTS_index$Geom_avg <- window$Geom_avg <- Geometric_averages
  Imputations$Index <- Matrix_HMTS$Index <- Matrix_HMTS_index$Index <- window$Index <- calculate_index(period_list, Geometric_averages, from_growth_rate = FALSE, reference_period = reference_period)

  if(is.null(state_space_model) == FALSE){
    Matrix_HMTS$Geom_avg <- Matrix_HMTS_index$Geom_avg <- Geometric_averages
    Matrix_HMTS$Index <- Matrix_HMTS_index$Index <- Imputations$Index
  }

  # Create table for index
  if(number_of_observations == TRUE){
    Results <- dplyr::select(Imputations, 'Period', 'Index', 'Number_observations')
  } else {
    Results <- dplyr::select(Imputations, 'Period', 'Index')
  }

  # Add resting point to output
  if(resting_points == TRUE){
    if(is.null(state_space_model) == FALSE){
      Results <- list(Index = Results
                      , Window = window
                      , Chosen_index_series = Imputations
                      , Matrix_HMTS_index = Matrix_HMTS_index
                      , Matrix_HMTS = Matrix_HMTS
                      , Matrix_HMTS_index = Matrix_HMTS_index
                      , Matrix_HMTS = Matrix_HMTS
                      , Matrix_HMTS_analyse = Matrix_HMTS_analyse)
    } else {
      Results <- list(Index = Results
                      , Window = window
                      , Chosen_index_series = Imputations
                      , Matrix_HMTS_index = Matrix_HMTS_index
                      , Matrix_HMTS = Matrix_HMTS)
    }
  }

  return(Results)

}

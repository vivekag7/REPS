
#' Calculate the trend line for a provided time series of numeric values
#'
### Helper functions for estimating state space model

#' Calculate the trend line with the state space method for a provided time series (chronological order is assumed).
#' The series are calculated with the package KFAS.
#'
#' @author Pim Ouwehand, Farley Ishaak
#' @param original_series = time series with values in chrolological order
#' @param periodicity = if month, then  12. If quarter, then 4, etc. (defaul = 4)
#' @param resting_points = Should analyses values be returned? (default = FALSE)
#' @return Trend line
#' @export
#' @examples
#' series <- c(85, 97, 100, 104, 111)
#' calculate_trend_line_KFAS(series)

calculate_trend_line_kfas <- function(original_series
                                      , periodicity
                                      , resting_points) {
  
  original_series <- log(original_series)
  
  origineel_ts <- stats::ts(original_series, start = 1, frequency = periodicity, names = "origineel_ts")
  
  startvalues_old <- set_startvalues(log(0.1), log(0.1), log(0.1), log(0.1), log(0.1))
  
  modelsel <- select_state_space_model(series = origineel_ts,
                                       initial_values_all = startvalues_old)
  
  model_ss <- model <- modelsel$model
  startvalues_temp <- modelsel$initial_values
  startvalues_temp <- determine_initial_parameters(model, initial_values = startvalues_temp)
  startvalues_new <- startvalues_temp$initial_values_2
  startvalues_analysis <- startvalues_temp$analysis_ts
  
  max_lik <- estimate_ts_parameters(model = model_ss, initial_values = startvalues_new)
  parameters_analysis <- max_lik$analyse_ts
  state_space_model <- smooth_ts(max_lik$fitmdl$model) #state_space_model
  model_analysis <- state_space_model$analyse_ts
  
  trend_line <- exp(state_space_model$signalsubconf[, 1])
  
  
  if (resting_points == TRUE) {
    analysis_complete <- bind_rows(startvalues_analysis, parameters_analysis, model_analysis)
    trend_line <- list(trend_line = trend_line, resting_points = analysis_complete)
  }
  
  return(trend_line)
}



#' Default update function
#'
#' This function is used in the function: calculate_trend_line_KFAS()
#'
#' @author Pim Ouwehand
#' @param pars = startvalues
#' @param model = state space modelnumber
#' @return newmodel
#' @keywords internal
#' @export
#' @examples
#' defaultupdatefn(pars = startvalues, model = 5)

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


#' Determine_initial_parameters
#'
#' Determine startvalues within state space models
#' This function is used in the function: calculate_trend_line_KFAS()
#'
#' @author Pim Ouwehand, Farley Ishaak
#' @param model = modelvalues as output of the function select_state_space_model()
#' @param initial_values = $initial.values as output of the model
#' @param FUN = function called: defaultupdatefn
#' @return New initial startvalues
#' @keywords internal
#' @export
#' @examples
#' startvalues_old <- set_startvalues(log(0.1), log(0.1), log(0.1),log(0.1),log(0.1))
#' modelsel <- select_state_space_model(selection = modelnumber
#'                                             , series = Origineel_TS
#'                                             , regressors = regressor
#'                                             , initial.values.all = startvalues_old)
#' model_SS <- model <- modelsel$model
#' startvalues_temp <- modelsel$initial.values
#' startvalues_temp <- determine_initial_parameters(model, initial.values = startvalues_temp)
#' startvalues_new <- startvalues_temp$initial.values2

determine_initial_parameters <- function(model, initial_values, FUN=defaultupdatefn) {
  
  # update the Q-matrix (only once)
  model2 <- FUN(initial_values, model)
  
  # Run the Kalman Filter in order to compute the LogLikelihood:
  kalman_filter <- KFAS::KFS(model2, filtering = "state", smoothing = "state")
  
  # Create table for analysis
  analysis_ts <- data.frame(reeks = character(), value = double())
  
  #1
  analysis_ts[1, ] <- c("Log likelihood startvalues", round(kalman_filter$logLik, 3))
  
  # in SsfPack the scale factor is returned from function 'SsfLikEx'
  # Here, we have to compute it manually:
  d <- sum(model2$P1inf) #number of diffuse initial states
  n <- length(model2$y) # number of observations
  ## David: deze variabelenamen voldoen niet aan snake case, maar deze gewoon laten staan. Zijn domein specifiek..
  v <- kalman_filter$v[(d + 1):n]
  F1 <- kalman_filter$F[(d + 1):n]^-1
  vF1v <- v * F1 * v  #v'_t * F_t^-1 * v_t
  
  # loglik according to KFAS Vignette section 2.1
  # univariate treatment and diffuse initialization, the diffuse log-likelihood is:
  log_lik_diffuse <- -0.5 * sum(log(kalman_filter$Finf)) -0.5 * sum(log(2 * pi) + log(kalman_filter$F[(d + 1):n]) + vF1v)
  # not needed here, but gives same result as KF$logLik
  # and confirms that vF1v is computed correctly, and is needed to compute the scale factor.
  difference_log_lik <- round(kalman_filter$logLik, 2) - round(log_lik_diffuse, 2)
  if (difference_log_lik != 0) {
    print(paste0("difference in LogLiks:",   kalman_filter$logLik, " vs. ", log_lik_diffuse))
  }
  
  # scale factor (see equation 11.7 in Commandeur and Koopman or 9.4 in Ssfpack manual, Koopman, Shepherd, Doornik, 2008)
  scale <- 1 / (n - d) * sum(vF1v)
  
  #2
  analysis_ts[2, ] <- c("Scale factor", round(scale, 5))
  
  #use scale factor to update initial values of hyperparameters:
  initial_values_2 <- initial_values + 0.5 * log(scale)
  
  #3
  analysis_ts[3, ] <- c("Slope", initial_values_2[1])
  analysis_ts[4, ] <- c("Meas", initial_values_2[2])
  
  return(list(initial_values_2 = initial_values_2, analysis_ts = analysis_ts))
}



#' Estimate time series parameters
#'
#' Estimate parameters to estimate trend lines
#' This function is used in the function: calculate_trend_line_KFAS()#'
#'
#' @author Pim Ouwehand, Farley Ishaak
#' @param model = model values as output of the function select_state_space_model()
#' @param initial_values = $initial.values as output of the model
#' @return parameter for the time series model
#' @keywords internal
#' @examples
#' estimate.TS.parameters(model = model, initial.values = startvalues)

estimate_ts_parameters <- function(model, initial_values){
  
  # original series, same as series
  original_series <- model$y
  
  fitmdl <- KFAS::fitSSM(model=model, inits=initial_values, method='BFGS', hessian=TRUE)
  log_lik_value <- -fitmdl$optim.out$value # provides logLik
  log_lik_value2 <- -fitmdl$optim.out$value / length(original_series) # as in Commandeur and Koopman
  
  q <- sum(model$P1inf) # number of diffuse initial values in the state
  w <- sum(is.na(model$Q)) + sum(is.na(model$H)) # total number of disturbance variances estimated
  
  aic <- (-2 * log_lik_value + 2 * (q + w))
  aic2 <- aic / length(original_series)  # as in Commandeur and Koopman
  
  LikAIC <- matrix(c(log_lik_value, aic, log_lik_value2, aic2), nrow=2,
                   dimnames=list(c("LogLik", "AIC"), c("value", "value/n")))
  
  # Store estimates in table
  analyse_ts <- data.frame(reeks = character(), value = double())
  analyse_ts[1, ] <- c('Log likelihood model', round(log_lik_value, 3))
  analyse_ts[2, ] <- c('Log likelihood model /n', round(log_lik_value2, 3))
  analyse_ts[3, ] <- c('AIC', round(aic, 3))
  analyse_ts[4, ] <- c('AIC /n', round(aic2, 3))
  
  # Store estimates in table
  analyse_ts[5, ] <- c('q (#diffuse states)', q)
  analyse_ts[6, ] <- c('w (#hyp.par)', w)
  
  sigma.sq <- exp(fitmdl$optim.out$par)  # maximum likelihood estimate of the stdev of the disturbances
  sigma    <- sqrt(sigma.sq)
  
  se <- fitmdl$optim.out$par*NA
  try({ H <- fitmdl$optim.out$hessian
  cov <- solve(-H)
  se  <- sqrt(diag(-cov)) #standard errors
  })
  
  tvalues <- abs(fitmdl$optim.out$par / se)
  
  volgorde <- c(length(sigma), 1:(length(sigma)-1))
  if(length(sigma)>1) volgorde <- c(length(sigma), 1:(length(sigma)-1))
  if(length(sigma)<=1) volgorde <- length(sigma)
  sigma2 <- sigma[volgorde]
  sigma.sq2 <- sigma.sq[volgorde]
  tvalues2 <- tvalues[volgorde]
  
  par.est <- t(rbind(sigma2, sigma.sq2, tvalues2))  #
  dimnames(par.est)[[2]] <- c("sigma", "sigma^2", "t-value") #column names
  
  # Store estimates in table
  analyse_ts[7, ] <- c("meas sigma", round(par.est[1,1], 3))
  analyse_ts[8, ] <- c("meas sigma^2", round(par.est[1,2], 4))
  analyse_ts[9, ] <- c("meas t-value", round(par.est[1,3], 3))
  analyse_ts[10, ] <- c("slope sigma", round(par.est[2,1], 3))
  analyse_ts[11, ] <- c("slope sigma^2", round(par.est[2,2], 4))
  analyse_ts[12, ] <- c("slope t-value", round(par.est[2,3], 3))
  
  return(list(fitmdl = fitmdl, LikAIC = LikAIC, sigma = sigma, analyse_ts = analyse_ts))
  
} 


#' Select the state space model type
#'
#' This function is used in the function: calculate_trend_line_KFAS()
#'
#' @author Pim Ouwehand
#' @param series = time series with values in chronological order
#' @param initial_values_all = =  startvalues for 5 hyperparameters: meas, level, slope, seas, scaling
#' @return modelvalues (level, slope) of the chosen state space model and the provided time series
#' @keywords internal
#' @export
#' @examples
#' # Create a quarterly time series
#' series <- ts(c(100, 102, 105, 110, 115, 120),
#'              start = c(2020, 1), frequency = 4)
#'
#' # Define initial values for meas, level, slope, seas, scaling
#' initial_values_all <- c(meas = 1, level = 0.5, slope = 0.1, seas = 0.2, scaling = 0.3)
#'
#' # Select the state space model
#' result <- select_state_space_model(series, initial_values_all)
#'
#' # Inspect the model and initial values
#' result$model
#' result$initial_values


select_state_space_model <- function(series, initial_values_all) {
  
  periodicity <- stats::tsp(series)[3]
  
  model <- KFAS::SSModel(series ~ SSMtrend(2, Q = list(0, matrix(NA))), H = matrix(NA))
  
  initial_values <- initial_values_all[c(3, 1)]
  
  return(list(model = model, initial_values = initial_values))
  
}


#' Set starting values for hyperparameters in state space models
#'
#' @return starting values for hyperparameters
#' @keywords internal
#' @export
#' @examples
#' set_startvalues(85, 97, 100, 104, 111)

set_startvalues <- function(a, b, c, d, e) {
  
  init <- c(a, b, c, d, e)
  names(init) <- c("meas", "level", "slope", "seas", "scaling")
  
  return(init = init)
  
}


#' Run forward and backward pass of time series estimation
#'
#' Calculate a trend line based on a provided model.
#' This function is used in the function: calculate_trend_line_KFAS()
#'
#' @author Pim Ouwehand, Farley Ishaak
#' @param fittedmodel = model values as output of the function estimate.TS.parameters()
#' @return sub-list $signalsubconf[,1] provides the estimated trend line
#' @keywords internal
#' @examples
#' smooth.TS(fittedmodel = model)

smooth_ts <- function(fittedmodel) {
  out_KFS    <- KFAS::KFS(fittedmodel, filtering='state', smoothing='state')
  signal_3 <- KFAS::signal(out_KFS, states="all")
  signal <- signal_3$signal
  signalvar <- signal_3$variance[1,1,]
  signalsubset <- KFAS::signal(out_KFS, states=c("level", "regression"))
  signalsub <- signalsubset$signal
  signalsubvar <- signalsubset$variance[1,1,]
  
  state <- out_KFS$alphahat
  # state[1,]
  statevar <- sapply(1:dim(out_KFS$V)[1], function(i) out_KFS$V[i,i,])
  StateSE <- sqrt(statevar) #defaultfouten
  # state / StateSE #t-values
  stvar <- matrix(statevar, nrow=dim(statevar)[1], dimnames=list(c(), dimnames(state)[[2]]))
  mStSmo <- cbind(state, signal, stvar, signalvar)
  # mStSmo
  
  state.est.1 <- t(rbind(state[1,], StateSE[1,], state[1,]/StateSE[1,]))
  dimnames(state.est.1)[[2]] <- c("Estimate", "Std.Error", "t-value") #column names
  
  # Store level and slope of 1th period in table
  analyse_ts <- data.frame(reeks = character(), value = double())
  analyse_ts[1, ] <- c('Level estimate (first period)', round(state.est.1[1,1], 4))
  analyse_ts[2, ] <- c('Level std.error (first period)', round(state.est.1[1,2], 4))
  analyse_ts[3, ] <- c('Level t-value (first period)', round(state.est.1[1,3], 4))
  analyse_ts[4, ] <- c('Slope estimate (first period)', round(state.est.1[2,1], 4))
  analyse_ts[5, ] <- c('Slope std.error (first period)', round(state.est.1[2,2], 4))
  analyse_ts[6, ] <- c('Slope t-value (first period)', round(state.est.1[2,3], 4))
  
  last <- dim(state)[1]
  state.est.n <- t(rbind(state[last,], StateSE[last,], state[last,]/StateSE[last,]))
  dimnames(state.est.n)[[2]] <- c("Estimate", "Std.Error", "t-value") #column names
  
  # Store level and slopt of lasth period in table
  analyse_ts[7, ] <- c('Level estimate (last period)', round(state.est.n[1,1], 4))
  analyse_ts[8, ] <- c('Level std.error (last period)', round(state.est.n[1,2], 4))
  analyse_ts[9, ] <- c('Level t-value (last period)', round(state.est.n[1,3], 4))
  analyse_ts[10, ] <- c('Slope estimate (last period)', round(state.est.n[2,1], 4))
  analyse_ts[11, ] <- c('Slope std.error (last period)', round(state.est.n[2,2], 4))
  analyse_ts[12, ] <- c('Slope t-value (last period)', round(state.est.n[2,3], 4))
  
  conf_series1 <- stats::predict(fittedmodel, interval = "confidence", level = 0.95)
  LB <- signal - stats::qnorm(0.975) * sqrt(signalvar)
  UB <- signal + stats::qnorm(0.975) * sqrt(signalvar)
  signalconf <- cbind(signal, LB, UB)
  # signalconf - conf_series1
  
  LBtrend <- signalsub - stats::qnorm(0.975) * sqrt(signalsubvar)
  UBtrend <- signalsub + stats::qnorm(0.975) * sqrt(signalsubvar)
  signalsubconf <- cbind(trend=signalsub, LB=LBtrend, UB=UBtrend)
  
  return(list(out_KFS=out_KFS, signal=signal, signalvar=signalvar, signalconf=signalconf,
              signalsub=signalsub, signalsubvar=signalsubvar, signalsubconf=signalsubconf,
              mStSmo=mStSmo, analyse_ts = analyse_ts))
  
}



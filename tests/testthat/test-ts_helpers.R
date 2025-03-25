test_that("Test calculate_trend_line_kfas", {
  series <- c(85, 97, 100, 104, 111)
  result <- calculate_trend_line_kfas(series, periodicity = 4, resting_points = FALSE)
  
  expect_type(result, "double")
  expect_length(result, length(series))
  expect_true(all(result > 0))
})

test_that("Test calculate_trend_line_kfas works", {
  series <- c(85, 97, 100, 104, 111)
  result <- calculate_trend_line_kfas(series, periodicity = 4, resting_points = TRUE)
  
  expect_type(result, "list")
  expect_named(result, c("trend_line", "resting_points"))
  expect_true(all(result$trend_line > 0))
  expect_s3_class(result$resting_points, "data.frame")
})

test_that("Test set_startvalues", {
  init <- set_startvalues(1, 2, 3, 4, 5)
  
  expect_type(init, "double")
  expect_named(init, c("meas", "level", "slope", "seas", "scaling"))
})

test_that("Test select_state_space_model", {
  series <- ts(c(100, 102, 105, 110, 115, 120), start = c(2020, 1), frequency = 4)
  init_vals <- set_startvalues(log(0.1), log(0.1), log(0.1), log(0.1), log(0.1))
  result <- select_state_space_model(series, init_vals)
  
  expect_type(result, "list")
  expect_s3_class(result$model, "SSModel")
  expect_length(result$initial_values, 2)
})

test_that("Test defaultupdatefn", {
  series <- ts(c(85, 97, 100, 104, 111), start = 1, frequency = 4)
  init_vals <- set_startvalues(log(0.1), log(0.1), log(0.1), log(0.1), log(0.1))
  model_data <- select_state_space_model(series, init_vals)
  model <- defaultupdatefn(model_data$initial_values, model_data$model)
  
  expect_s3_class(model, "SSModel")
  expect_false(any(is.na(model$Q)))
  expect_false(any(is.na(model$H)))
})

test_that("Test determine_initial_parameters", {
  series <- ts(log(c(85, 97, 100, 104, 111)), start = 1, frequency = 4)
  init_vals <- set_startvalues(log(0.1), log(0.1), log(0.1), log(0.1), log(0.1))
  model_data <- select_state_space_model(series, init_vals)
  result <- determine_initial_parameters(model_data$model, model_data$initial_values)
  
  expect_type(result, "list")
  expect_named(result, c("initial_values_2", "analysis_ts"))
  expect_true(is.numeric(result$initial_values_2))
  expect_s3_class(result$analysis_ts, "data.frame")
})

test_that("Test estimate_ts_parameters", {
  series <- ts(log(c(85, 97, 100, 104, 111)), start = 1, frequency = 4)
  init_vals <- set_startvalues(log(0.1), log(0.1), log(0.1), log(0.1), log(0.1))
  model_data <- select_state_space_model(series, init_vals)
  param_data <- determine_initial_parameters(model_data$model, model_data$initial_values)
  result <- estimate_ts_parameters(model_data$model, param_data$initial_values_2)
  
  expect_type(result, "list")
  expect_s3_class(result$fitmdl$model, "SSModel")
  expect_true(is.matrix(result$LikAIC))
  expect_s3_class(result$analyse_ts, "data.frame")
})

test_that("Test smooth_ts", {
  series <- ts(log(c(85, 97, 100, 104, 111)), start = 1, frequency = 4)
  init_vals <- set_startvalues(log(0.1), log(0.1), log(0.1), log(0.1), log(0.1))
  model_data <- select_state_space_model(series, init_vals)
  param_data <- determine_initial_parameters(model_data$model, model_data$initial_values)
  ts_params <- estimate_ts_parameters(model_data$model, param_data$initial_values_2)
  smoothed <- smooth_ts(ts_params$fitmdl$model)
  
  expect_type(smoothed, "list")
  expect_true("signalconf" %in% names(smoothed))
  expect_true("signalsubconf" %in% names(smoothed))
  expect_s3_class(smoothed$analyse_ts, "data.frame")
})

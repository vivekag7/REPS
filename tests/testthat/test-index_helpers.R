test_that("Test prepare_hedonic_data", {
  # Create a dummy dataset with an empty string, an NA, and an extra unused column
  dummy_data <- data.frame(
    period = c("2020Q1", "2020Q1", "2020Q2", ""),
    price = c(100000, 200000, 150000, 300000),
    area = c(50, 100, NA, 80),
    condition = c("Good", "Bad", "Good", "Good"),
    unused_column = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  
  # Test 1: Standard cleaning (log_dependent = FALSE)
  clean_df <- prepare_hedonic_data(
    dataset = dummy_data, 
    period_variable = "period", 
    dependent_variable = "price", 
    numerical_variables = c("area"), 
    categorical_variables = c("condition"), 
    log_dependent = FALSE
  )
  
  # Check that unused column was dropped
  expect_false("unused_column" %in% names(clean_df))
  # Check NA and empty string rows were dropped (Rows 3 and 4)
  expect_equal(nrow(clean_df), 2)
  # Check variable coercion
  expect_true(is.factor(clean_df$condition))
  expect_true(is.character(clean_df$period))
  
  # Test 2: Log transformation
  clean_df_log <- prepare_hedonic_data(
    dataset = dummy_data, 
    period_variable = "period", 
    dependent_variable = "price", 
    numerical_variables = c("area"), 
    categorical_variables = c("condition"), 
    log_dependent = TRUE
  )
  expect_true("log_price" %in% names(clean_df_log))
  expect_equal(clean_df_log$log_price[1], log(100000))
  
  # Test 3: Stop execution if logging non-positive values
  dummy_data$price[1] <- 0
  expect_error(
    prepare_hedonic_data(dummy_data, "period", "price", c("area"), c("condition"), log_dependent = TRUE),
    "non-positive values"
  )
})

test_that("Test format_index_output", {
  periods <- c("2020Q1", "2020Q2", "2020Q3")
  values <- c(100, 105, 110)
  obs <- c(500, 520, 510)
  
  # Test 1: Base output (no observations, no rebasing)
  res1 <- format_index_output(periods, values)
  expect_equal(names(res1), c("period", "Index"))
  expect_equal(res1$Index, c(100, 105, 110))
  
  # Test 2: With observations (checks exact column order)
  res2 <- format_index_output(periods, values, observation_counts = obs)
  expect_equal(names(res2), c("period", "number_of_observations", "Index"))
  expect_equal(res2$number_of_observations, obs)
  
  # Test 3: With rebasing
  res3 <- format_index_output(periods, values, reference_period = "2020Q2")
  expect_equal(round(res3$Index[2], 0), 100) # Base period becomes 100
})

test_that("Test fit_hedonic_model and predict_hedonic", {
  # Create simple linear relationship
  dummy_data <- data.frame(
    log_price = c(10, 10.5, 11, 11.5, 12),
    area = c(50, 60, 70, 80, 90),
    city = factor(c("A", "A", "B", "B", "C"))
  )
  
  # Test 1: Fit model with independent variables
  model <- fit_hedonic_model(dummy_data, "log_price", c("area", "city"))
  expect_s3_class(model, "lm")
  expect_true(length(coef(model)) > 2) # Intercept + area + dummies
  
  # Test 2: Fit empty/null model
  model_null <- fit_hedonic_model(dummy_data, "log_price", character(0))
  expect_s3_class(model_null, "lm")
  expect_equal(length(coef(model_null)), 1) # Intercept only
  
  # Test 3: Predictions
  preds <- predict_hedonic(model, newdata = dummy_data)
  expect_equal(length(preds), 5)
  expect_true(is.numeric(preds))
  
  # Predictions from lm object should match standard stats::predict
  expect_equal(as.numeric(preds), as.numeric(stats::predict(model, dummy_data)))
})
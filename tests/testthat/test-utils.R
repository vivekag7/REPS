test_that("Test validate_input", {
  # Valid input should not throw an error or warning
  expect_silent(validate_input(
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ))
  
  # Error: missing period column should throw an error
  expect_error(validate_input(
    dataset = data_constraxion[, -which(names(data_constraxion) == "period")],
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ), "Dataset is missing the following required column")
  
  # Error: non-numeric numerical variable should throw an error
  data_non_numeric <- data_constraxion
  data_non_numeric$floor_area <- as.character(data_non_numeric$floor_area)
  expect_error(validate_input(
    dataset = data_non_numeric,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ), "is not numeric")
  
  # Error: negative price should throw an error (log transformation requirement)
  data_negative_price <- data_constraxion
  data_negative_price$price[1] <- -100000
  expect_error(validate_input(
    dataset = data_negative_price,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ), "contains zero or negative values")
  
  # Warning: invalid period format should trigger a warning advising correction
  data_bad_period <- data_constraxion
  data_bad_period$period <- c("2020-01", "2020-02", rep("2020Q1", nrow(data_bad_period) - 2))
  expect_warning(validate_input(
    dataset = data_bad_period,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ), "The period variable contains values that do not match a recognized format")
  
  # Error: both numerical and categorical variables are NULL
  expect_error(validate_input(
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = NULL,
    categorical_variables = NULL
  ), "Both numerical_variables and categorical_variables are missing or empty")
})

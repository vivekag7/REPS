test_that("Test calculate_method_index", {
  expect_error(
    calculate_method_index(
      method = "invalid",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code"
    ),
    "Invalid method: 'invalid'"
  )
  
  expect_silent(
    calculate_method_index(
      method = "laspeyres",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      log_dependent = TRUE,
      reference_period = 2015,
      number_of_observations = TRUE,
      imputation = TRUE
    )
  )
})


test_that("Test calculate_price_index", {
  # Bad input
  expect_error(
    calculate_price_index(
      method = "invalid",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code"
    ),
    "Invalid method: 'invalid'"
  )
  
  # Laspeyres
  expect_silent(
    calculate_price_index(
      method = "laspeyres",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      number_of_observations = TRUE,
      imputation = TRUE
    )
  )
  
  # Paasche
  expect_silent(
    calculate_price_index(
      method = "paasche",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      number_of_observations = TRUE,
      imputation = FALSE
    )
  )
  
  # Fisher
  expect_silent(
    calculate_price_index(
      method = "fisher",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      number_of_observations = TRUE
    )
  )
  
  # HMTS
  expect_silent(
    calculate_price_index(
      method = "hmts",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      number_of_observations = TRUE,
      periods_in_year = 4,
      production_since = NULL,
      number_preliminary_periods = 2,
      resting_points = FALSE
    )
  )
})

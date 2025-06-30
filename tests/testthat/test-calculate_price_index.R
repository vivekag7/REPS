test_that("Test calculate_price_index", {
  
  # Invalid method should error
  expect_error(
    calculate_price_index(
      method = "invalid",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code"
    ),
    "Invalid method"
  )
  
  # Single method should work
  expect_silent(
    calculate_price_index(
      method = "fisher",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      continuous_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = "2015Q1"
    )
  )
  
  # Multiple methods (no HMTS) should work
  result <- calculate_price_index(
    method = c("fisher", "paasche", "timedummy"),
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = "2015Q1"
  )
  
  expect_type(result, "list")
  expect_named(result, c("fisher", "paasche", "timedummy"))
  
  
})

test_that("Testplot_price_index ", {
  result <- calculate_price_index(
    method = c("fisher", "paasche"),
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = "2015Q1"
  )
  
  expect_silent(plot_price_index(result))
})


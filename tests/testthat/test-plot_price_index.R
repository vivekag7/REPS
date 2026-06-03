test_that("Test plot_price_index", {

  # 1. Normal plotting should work silently
  result_normal <- calculate_hedonic_index(
    method = c("fisher", "paasche"),
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015
  )

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_silent(plot_price_index(result_normal))

  # 2. Plotting HMTS resting_points list should trigger custom error
  result_resting <- calculate_hedonic_index(
    method = "hmts",
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015,
    number_preliminary_periods = 2,
    production_since = NULL,
    resting_points = TRUE
  )

  expect_error(
    plot_price_index(result_resting),
    "valid multi-method output"
  )
})
test_that("Test calculate_hedonic_index", {
  save_refs <- FALSE
  ref_file <- test_path("test_data", "chained_fisher_output.rds")

  # Reference output check of chained index method
  tbl_chained <- calculate_hedonic_index(
    method = "fisher",
    chained = TRUE, 
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015
  )

  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_chained, ref_file)
    succeed("Reference file saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_chained, ref_tbl, tolerance = 1e-8)
  }

  # Invalid method should error
  expect_error(
    calculate_hedonic_index(
      method = "invalid",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code"
    ),
    "Invalid method"
  )

  # Single method should work
  expect_silent(
    calculate_hedonic_index(
      method = "fisher",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015
    )
  )

  # Multiple methods (no HMTS) should work
  result <- calculate_hedonic_index(
    method = c("fisher", "paasche", "timedummy"),
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015
  )

  expect_type(result, "list")
  expect_named(result, c("fisher", "paasche", "timedummy"))

  # Multiple methods with resting_points = TRUE should error
  expect_error(
    calculate_hedonic_index(
      method = c("fisher", "hmts"),
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      periods_in_year = 4,
      number_preliminary_periods = 2,
      resting_points = TRUE
    ),
    "resting_points = TRUE"
  )
})

test_that("Test plot_price_index", {
  result <- calculate_hedonic_index(
    method = c("fisher", "paasche"),
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015
  )

  expect_silent(plot_price_index(result))
})
test_that("Test calculate_hedonic_index", {
  
  # 1. Chained Fisher reference output should match saved reference
  save_refs <- FALSE
  ref_file <- test_path("test_data", "chained_fisher_output.rds")
  
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
  
  # 2. Invalid method should error
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
  
  # 3. Single method should work
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
  
  # 4. Multiple methods without HMTS should work
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
  
  # 5. Multiple methods with resting_points = TRUE should error
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
  
  # 6. Chained index with resting_points = TRUE should error
  expect_error(
    calculate_hedonic_index(
      method = "hmts",
      chained = TRUE,
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
    "chained = TRUE"
  )
  
  # 7. RTD missing window_length triggers message
  expect_message(
    calculate_hedonic_index(
      method = "rolling_timedummy",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015
    ),
    "A default value of 5 has been applied"
  )
  
  # 8. HMTS missing parameters triggers messages
  expect_message(
    expect_message(
      calculate_hedonic_index(
        method = "hmts",
        dataset = data_constraxion,
        period_variable = "period",
        dependent_variable = "price",
        numerical_variables = "floor_area",
        categorical_variables = "neighbourhood_code",
        reference_period = 2015
      ),
      "number_preliminary_periods"
    ),
    "production since"
  )
  
  # 9. Repricing missing periods_in_year triggers error
  expect_error(
    calculate_hedonic_index(
      method = "repricing",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015
    ),
    "periods_in_year"
  )
})


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
    resting_points = TRUE
  )
  
  expect_error(
    plot_price_index(result_resting),
    "valid multi-method output"
  )
})
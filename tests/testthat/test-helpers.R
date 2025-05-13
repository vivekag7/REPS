test_that("Test calculate_geometric_average", {
  a <- c(1, 2, 3, 4)
  expect_equal(round(calculate_geometric_average(a), 4), round(2.213364, 4))
  
  # Check that NA values are ignored
  b <- c(1, 2, NA, 4)
  expect_equal(round(calculate_geometric_average(b), 4), round(exp(mean(log(c(1, 2, 4)))), 4))
  
})

test_that("Test calculate_index", {
  periods <- c("2020Q1", "2020Q2", "2020Q3")
  values <- c(100, 110, 90)
  
  result <- calculate_index(periods, values, reference_period = "2020Q1")
  expect_equal(round(result[1], 1), 100)
  expect_equal(round(result[2], 1), 110)
  expect_equal(round(result[3], 1), 90)
  
  # Check fallback to first period if reference not specified
  result2 <- calculate_index(periods, values)
  expect_equal(result, result2)
  
  # Non-numeric values trigger an error
  expect_error(calculate_index(periods, c("a", "b", "c")), "values variable is not \\(fully\\) numeric")
  
  # Mismatch in vector lengths triggers an error
  expect_error(calculate_index(c("2020Q1", "2020Q2"), c(100, 110, 120)), "not of the same length")
})


test_that("Test validate_input", {
  # Valid input should not throw an error or warning
  expect_silent(validate_input(
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ))
  
  # Error: missing period column should now throw an error (hard stop)
  expect_error(validate_input(
    dataset = data_constraxion[, -which(names(data_constraxion) == "period")],
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ), "Dataset is missing the following required column")
  
  # Error: non-numeric continuous variable should throw an error
  data_non_numeric <- data_constraxion
  data_non_numeric$floor_area <- as.character(data_non_numeric$floor_area)
  expect_error(validate_input(
    dataset = data_non_numeric,
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ), "is not \\(fully\\) numeric")
  
  # Error: negative price should throw an error (log transformation requirement)
  data_negative_price <- data_constraxion
  data_negative_price$price[1] <- -100000
  expect_error(validate_input(
    dataset = data_negative_price,
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ), "contains zero or negative values")
  
  # Warning: invalid period format should trigger a warning advising correction
  data_bad_period <- data_constraxion
  data_bad_period$period <- c("2020-01", "2020-02", rep("2020Q1", nrow(data_bad_period) - 2))
  expect_warning(validate_input(
    dataset = data_bad_period,
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = "floor_area",
    categorical_variables = "neighbourhood_code"
  ), "The period variable contains values that do not follow a recognized format")
})



test_that("Test calculate_hedonic_imputation", {
  save_refs <- FALSE  # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "hedonic_imputation_output.rds")
  
  # Input variables
  period_variable <- c("period")
  dependent_variable <- c("price")
  continuous_variables <- c("floor_area")
  categorical_variables <- c("neighbourhood_code")
  independent_variables <- c(continuous_variables, categorical_variables)
  number_of_observations <- TRUE
  
  
  # Prepare dataset in right format
  dataset <- data_constraxion |>
    dplyr::rename(period_var_temp = dplyr::all_of(period_variable)) |>
    dplyr::mutate(
      period_var_temp = as.character(period_var_temp),
      dplyr::across(dplyr::all_of(categorical_variables), as.factor)
    )
  
  period_list <- sort(unique(dataset$period_var_temp), decreasing = FALSE)
  
  # Run function
  tbl_output <- calculate_hedonic_imputation(
    dataset_temp = dataset,
    period_temp = "period_var_temp",
    dependent_variable_temp = dependent_variable,
    independent_variables_temp = independent_variables,
    number_of_observations_temp = number_of_observations,
    period_list_temp = period_list
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_output, ref_file)
    succeed("Reference output saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_output, ref_tbl, tolerance = 1e-8)
  }
})

test_that("Test calculate_hmts_index", {
  save_refs <- FALSE  # Set to TRUE to save the reference output
  ref_file <- test_path("test_data", "hmts_index_output.rds")
  
  # Parameters
  period_variable <- "period"
  dependent_variable <- "price"
  continuous_variables <- c("floor_area")
  categorical_variables <- c("neighbourhood_code")
  reference_period <- 2015
  periods_in_year <- 4
  production_since <- NULL
  number_preliminary_periods <- 2
  number_of_observations <- TRUE
  resting_points <- FALSE  # Set to FALSE as requested
  
  # Prepare dataset as expected by the function
  dataset <- data_constraxion |>
    dplyr::rename(period = dplyr::all_of(period_variable)) |>
    dplyr::mutate(
      period = as.character(period),
      dplyr::across(dplyr::all_of(categorical_variables), as.factor)
    )
  
  # Run the function
  tbl_output <- calculate_hmts_index(
    dataset = dataset,
    period_variable = period_variable,
    dependent_variable = dependent_variable,
    continuous_variables = continuous_variables,
    categorical_variables = categorical_variables,
    reference_period = reference_period,
    periods_in_year = periods_in_year,
    production_since = production_since,
    number_preliminary_periods = number_preliminary_periods,
    number_of_observations = number_of_observations,
    resting_points = resting_points
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_output, ref_file)
    succeed("Reference output saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_output, ref_tbl, tolerance = 1e-8)
  }
})

test_that("Test calculate_hedonic_imputationmatrix", {
  save_refs <- FALSE  # Set to TRUE to save the reference output
  ref_file <- test_path("test_data", "hedonic_imputation_matrix_output.rds")
  
  # Parameters used by the function
  period_variable <- "period"
  dependent_variable <- "price"
  continuous_variables <- c("floor_area")
  categorical_variables <- c("neighbourhood_code")
  periods_in_year <- 4
  number_of_observations <- TRUE
  production_since <- NULL
  number_preliminary_periods <- 2
  
  # Prepare dataset as expected
  dataset <- data_constraxion |>
    dplyr::rename(period = dplyr::all_of(period_variable)) |>
    dplyr::mutate(
      period = as.character(period),
      dplyr::across(dplyr::all_of(categorical_variables), as.factor)
    )
  
  # Run the function
  matrix_output <- calculate_hedonic_imputationmatrix(
    dataset = dataset,
    period_variable = "period",
    dependent_variable = dependent_variable,
    continuous_variables = continuous_variables,
    categorical_variables = categorical_variables,
    periods_in_year = periods_in_year,
    number_of_observations = number_of_observations,
    production_since = production_since,
    number_preliminary_periods = number_preliminary_periods
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(matrix_output, ref_file)
    succeed("Reference output saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
  
    
    expect_equal(matrix_output$matrix_hmts_index, ref_tbl$matrix_hmts_index, tolerance = 1e-3)
  }
})

test_that("Test calculate_growth_rate", {
  values <- c(100, 110, 121)
  growth <- calculate_growth_rate(values)
  
  expect_equal(round(growth[1], 3), 1.000)
  expect_equal(round(growth[2], 3), 1.100)
  expect_equal(round(growth[3], 3), 1.100)
  
  expect_error(calculate_growth_rate(c("a", "b", "c")), "not fully numeric")
})



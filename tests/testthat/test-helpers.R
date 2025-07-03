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

test_that("Test calculate_hedonic_imputation", {
  save_refs <- FALSE  # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "hedonic_imputation_output.rds")
  
  # Input variables
  period_variable <- c("period")
  dependent_variable <- c("price")
  numerical_variables <- c("floor_area")
  categorical_variables <- c("neighbourhood_code")
  independent_variables <- c(numerical_variables, categorical_variables)
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



test_that("Test calculate_growth_rate", {
  values <- c(100, 110, 121)
  growth <- calculate_growth_rate(values)
  
  expect_equal(round(growth[1], 3), 1.000)
  expect_equal(round(growth[2], 3), 1.100)
  expect_equal(round(growth[3], 3), 1.100)
  
  expect_error(calculate_growth_rate(c("a", "b", "c")), "not fully numeric")
})



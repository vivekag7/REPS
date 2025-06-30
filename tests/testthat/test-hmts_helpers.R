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
  diagnostics <- FALSE 
  
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
    diagnostics = diagnostics
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
  diagnostics <- FALSE
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
    diagnostics = diagnostics,
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

test_that("Test calculate_chained_index", {
  
  save_refs <- FALSE # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "chained_fisher_output.rds")
  
  # 1. Invalid method should error
  expect_error(
    calculate_chained_index(
      method = "invalid",
      dataset = data_constraxion,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code"
    )
  )
  
  # 2. Reference output check
  tbl_chained <- calculate_chained_index(
    method = "fisher",
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
})
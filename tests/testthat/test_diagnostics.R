test_that("Test Diagnostics Calculation", {
  save_refs <- FALSE  # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "diagnostics_output.rds")
  
  diagnostics_tbl <- calculate_regression_diagnostics(
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = c("floor_area", "dist_trainstation"),
    categorical_variables = c("dummy_large_city", "neighbourhood_code")
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(diagnostics_tbl, ref_file)
    succeed("Reference file saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(diagnostics_tbl, ref_tbl, tolerance = 1e-8)
  }
})

test_that("Test Rolling Time Dummy", {
  save_refs <- FALSE  # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "rolling_timedummy_output.rds")
  
  tbl_rolling <- calculate_rolling_timedummy_index(
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = "2015",
    window_length = 5,
    diagnostics = FALSE
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_rolling, ref_file)
    succeed("Reference file saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_rolling, ref_tbl, tolerance = 1e-8)
  }
})

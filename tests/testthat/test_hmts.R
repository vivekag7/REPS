test_that("Test HMTS", {
  save_refs <- FALSE  # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "hmts_output.rds")
  
  tbl_hmts <- calculate_hmts(
    dataset = data_constraxion,
    period_variable = c("period"),
    dependent_variable = c("price"),
    continuous_variables = c("floor_area"),
    categorical_variables = c("neighbourhood_code"),
    reference_period = 2015,
    periods_in_year = 4,
    production_since = NULL,
    number_preliminary_periods = 2,
    number_of_observations = TRUE,
    resting_points = TRUE
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_hmts, ref_file)
    succeed("Reference file saved.")  
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_hmts$Index, ref_tbl$Index, tolerance = 1e-3)
  }
})

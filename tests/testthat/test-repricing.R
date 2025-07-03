test_that("Test Repricing", {
  save_refs <- FALSE  # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "repricing_output.rds")
  
  tbl_repricing <- calculate_repricing(
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    continuous_variables = c("floor_area"),
    categorical_variables = c("neighbourhood_code"),
    reference_period = 2015,
    number_of_observations = FALSE
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_repricing, ref_file)
    succeed("Reference file saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_repricing, ref_tbl, tolerance = 1e-8)
  }
})

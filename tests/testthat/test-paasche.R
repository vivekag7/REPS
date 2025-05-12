test_that("Test Paasche", {
  save_refs <- FALSE  # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "paasche_output.rds")
  
  tbl_paasche <- calculate_paasche(
    dataset = data_constraxion,
    period_variable = c("period"),
    dependent_variable = c("price"),
    continuous_variables = c("floor_area"),
    categorical_variables = c("neighbourhood_code"),
    reference_period = 2015,
    number_of_observations = FALSE,
    imputation = TRUE
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_paasche, ref_file)
    succeed("Reference file saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_paasche, ref_tbl, tolerance = 1e-8)
  }
})

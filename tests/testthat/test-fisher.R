test_that("Test Fisher", {
  save_refs <- FALSE  # Set to TRUE to save reference output
  ref_file <- test_path("test_data", "fisher_output.rds")
  
  tbl_fisher <- calculate_fisher(
    dataset = data_constraxion,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015,
    number_of_observations = FALSE
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_fisher, ref_file)
    succeed("Reference file saved.")  
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_fisher, ref_tbl, tolerance = 1e-8)
  }
})

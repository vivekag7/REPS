# Self-contained regression test: expected values were generated independently
# and stored in test_data/spar_reference_output.rds. No external files needed.

expect_spar_reference_values <- function(expected, actual, tolerance = 1e-10) {
  keys <- c("period", "appraisal_year", "property_type")
  expect_false(anyDuplicated(expected[keys]) > 0L)
  expect_false(anyDuplicated(actual[keys]) > 0L)
  joined <- merge(expected, actual[names(expected)], by = keys, all = TRUE,
                  suffixes = c("_reference", "_actual"))
  expect_equal(nrow(joined), nrow(expected))
  expect_equal(nrow(joined), nrow(actual))
  for (column in setdiff(names(expected), keys)) {
    difference <- abs(joined[[paste0(column, "_actual")]] - joined[[paste0(column, "_reference")]])
    expect_false(anyNA(difference), info = column)
    expect_true(all(difference <= tolerance), info = paste(column, "maximum absolute difference:", max(difference)))
  }
}

test_that("arithmetic_data matches the independently generated script expectations", {
  data("arithmetic_data", package = "REPS")
  fixture <- readRDS(test_path("test_data", "spar_reference_output.rds"))
  result <- calculate_spar_index(
    arithmetic_data, c("arithmetic", "geometric", "unweighted"),
    period_variable = "period", dependent_variable = "price",
    appraisal_variable = "appraisal_value", grouping_variables = c("appraisal_year", "property_type"),
    index_type_variable = "property_type", chained = TRUE
  )
  for (method in names(fixture$expected)) {
    expect_spar_reference_values(fixture$expected[[method]], result[[method]])
  }
})

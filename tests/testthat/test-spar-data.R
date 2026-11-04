test_that("the original documented SPAR fixture remains available", {
  data("aritmethic_data", package = "REPS")
  expect_s3_class(aritmethic_data, "data.frame")
  expect_equal(dim(aritmethic_data), c(360L, 5L))
  expect_false(anyNA(aritmethic_data))
  result <- calculate_spar(
    aritmethic_data, c("arithmetic", "geometric", "unweighted"),
    period_variable = "Period", dependent_variable = "Price",
    appraisal_variable = "Appraisal Value",
    grouping_variables = c("Appraisal Year", "Property Type"),
    chained = TRUE, index_type_variable = "Property Type"
  )
  expect_true(all(vapply(result, nrow, integer(1)) == 120L))
})

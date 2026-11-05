test_that("arithmetic_data preserves the complete source time pattern in a compact example", {
  data("arithmetic_data", package = "REPS")
  expect_s3_class(arithmetic_data, "data.frame")
  expect_equal(dim(arithmetic_data), c(857L, 5L))
  expect_named(arithmetic_data, c("period", "appraisal_year", "price", "property_type", "appraisal_value"))
  expect_type(arithmetic_data$period, "integer")
  expect_type(arithmetic_data$appraisal_year, "integer")
  expect_type(arithmetic_data$price, "double")
  expect_type(arithmetic_data$property_type, "character")
  expect_type(arithmetic_data$appraisal_value, "double")
  expect_false(anyNA(arithmetic_data))
  expect_true(all(arithmetic_data$price > 0 & arithmetic_data$appraisal_value > 0))
  expect_equal(range(arithmetic_data$period), c(201301L, 201812L))
  expect_equal(sort(unique(arithmetic_data$appraisal_year)), 2011:2016)
  expect_equal(sort(unique(arithmetic_data$property_type)),
    c("Apartment", "End-of-terrace house", "Mid-terrace house", "Semi-detached house"))
  counts <- table(arithmetic_data$period, arithmetic_data$property_type)
  expect_equal(dim(counts), c(72L, 4L))
  expect_true(all(counts >= 1L & counts <= 3L))
  expect_equal(as.integer(table(arithmetic_data$appraisal_year)), c(142L, 144L, 139L, 144L, 144L, 144L))
  expect_equal(as.integer(table(arithmetic_data$property_type)), c(216L, 216L, 216L, 209L))
  for (type in unique(arithmetic_data$property_type)) {
    series <- unique(arithmetic_data[arithmetic_data$property_type == type, c("period", "appraisal_year")])
    series <- series[order(series$period), ]
    expect_equal(sum(diff(series$appraisal_year) != 0), 5L)
    expect_equal(sum(series$period %% 100 == 1), 6L)
    expect_equal(sum(series$period %% 100 == 12), 6L)
  }
})

test_that("arithmetic_data works with every public SPAR method and plotting", {
  data("arithmetic_data", package = "REPS")
  result <- calculate_spar_index(
    arithmetic_data, c("spar_arithmetic", "spar_geometric", "spar_unweighted"),
    period_variable = "period", dependent_variable = "price",
    appraisal_variable = "appraisal_value",
    grouping_variables = c("appraisal_year", "property_type"),
    chained = TRUE, index_type_variable = "property_type"
  )
  expect_named(result, c("spar_arithmetic", "spar_geometric", "spar_unweighted"))
  expect_true(all(vapply(result, nrow, integer(1)) == 288L))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot_price_index(result), "filter grouped output")
  filtered <- lapply(result, function(x) x[x$property_type == "Apartment", ])
  expect_silent(plot_price_index(filtered))
})

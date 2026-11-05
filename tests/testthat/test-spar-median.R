spar_median_data <- function() {
  data.frame(
    period = rep(rep(c(202101, 202112, 202201, 202202), each = 3), 2),
    vintage = rep(rep(c(2019, 2019, 2020, 2020), each = 3), 2),
    type = rep(c("A", "B"), each = 12),
    price = rep(c(100, 200, 900), 8) * rep(c(1, 1.2, 3, 3.3, 1, 1.5, 4, 3.2), each = 3),
    appraisal = rep(c(80, 120, 220), 8)
  )
}

test_that("the SPAR hub offers the existing price-only median baseline", {
  dataset <- spar_median_data()
  dataset <- dataset[dataset$type == "A", ]
  result <- calculate_spar_index(dataset, "median", "period", "price")
  expect_equal(result, calculate_median(dataset, "period", "price"))
  expect_equal(result$Index, c(100, 120, 300, 330))
  dataset$appraisal <- NA_real_
  dataset$unused_predictor <- NA_real_
  expect_equal(calculate_spar_index(dataset, "median", "period", "price"), result)
  dataset$price[1] <- NA_real_
  expect_equal(calculate_spar_index(dataset, "median", "period", "price")$Index[1], 100)
  expect_identical(get_spar_index_method_function("median"), calculate_spar_median)
})

test_that("median compares independent series without the SPAR chain formula", {
  dataset <- spar_median_data()
  args <- list(dataset = dataset, method = c("spar_arithmetic", "median"),
    period_variable = "period", dependent_variable = "price", appraisal_variable = "appraisal",
    grouping_variables = c("vintage", "type"), index_type_variable = "type", chained = TRUE)
  result <- do.call(calculate_spar_index, args)
  expect_named(result, c("spar_arithmetic", "median"))
  expect_equal(result$median$Index, c(100, 120, 300, 330, 100, 150, 400, 320))
  expect_false("vintage" %in% names(result$median))
  spar_only <- args
  spar_only$method <- "spar_arithmetic"
  expect_equal(result$spar_arithmetic, do.call(calculate_spar_index, spar_only))
  parallel <- do.call(calculate_spar_index, c(args, list(parallel = TRUE)))
  expect_equal(parallel, result)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot_price_index(lapply(result, function(x) x[x$type == "A", ])))
})

test_that("median-only validation requires no appraisal or vintage columns", {
  dataset <- spar_median_data()[c("period", "price", "type")]
  result <- calculate_spar_index(dataset, "median", "period", "price", grouping_variables = "type")
  expect_equal(nrow(result), 8L)
  expect_error(calculate_spar_index(dataset, c("spar_arithmetic", "median"), "period", "price",
    grouping_variables = "type"), "appraisal_variable")
  expect_error(calculate_spar_index(dataset, "median", "period", "price",
    grouping_variables = "missing"), "missing median grouping")
  dataset$price <- NA_real_
  expect_error(calculate_spar_index(dataset, "median", "period", "price"), "complete price-period")
})

test_that("median supports the same contribution wrapper without requiring groups", {
  dataset <- data.frame(period = rep(c(202101, 202102), each = 3), price = c(10, 20, 90, 20, 40, 200))
  result <- calculate_spar_index(dataset, "median", "period", "price", index_contribution = TRUE)
  expect_named(result, c("Index", "Index_contribution"))
  expect_equal(result$Index$Index, c(100, 200))
  row <- result$Index_contribution[result$Index_contribution$row_id == "4", ]
  expect_equal(row$Index_excl_observation, 600)
  expect_equal(row$Index_difference, -400)
  dataset$unit <- "all"
  unavailable <- calculate_spar_index(dataset, "median", "period", "price",
    index_contribution = TRUE, unit_variable = "unit")
  expect_true(all(is.na(unavailable$Index_contribution$Index_excl_observation)))
})

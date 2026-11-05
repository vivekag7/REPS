spar_mean_data <- function() {
  data.frame(
    period = c(rep(202101, 3), rep(202102, 4), rep(202103, 3)),
    price = c(10, 20, 30, 20, 30, 50, 100, 60, 80, 100)
  )
}

test_that("mean indexes untransformed arithmetic averages without appraisal inputs", {
  dataset <- spar_mean_data()
  result <- calculate_spar_index(dataset, "mean", "period", "price")
  # Period means are 20, 50, 80; the even-sample second-period median is 40.
  expect_equal(result, data.frame(period = c("202101", "202102", "202103"), Index = c(100, 250, 400)))
  median <- calculate_spar_index(dataset, "median", "period", "price")
  expect_equal(median$Index, c(100, 200, 400))
  expect_identical(get_spar_index_method_function("mean"), calculate_spar_mean)
  expect_equal(validate_spar_index_methods("MEAN"), "mean")
  expect_equal(calculate_spar_index(dataset[nrow(dataset):1, ], "mean", "period", "price"), result)
})

test_that("mean ignores missing unused columns and excludes missing prices", {
  dataset <- spar_mean_data()
  dataset$appraisal <- NA_real_
  dataset$unused_predictor <- NA_real_
  dataset <- rbind(dataset, data.frame(period = 202102, price = NA_real_, appraisal = NA_real_, unused_predictor = NA_real_))
  expect_equal(calculate_spar_index(dataset, "mean", "period", "price")$Index, c(100, 250, 400))
  dataset$price <- NA_real_
  expect_error(calculate_spar_index(dataset, "mean", "period", "price"), "Mean index calculation requires")
})

test_that("mean uses independent grouping and the same scale options as median", {
  dataset <- spar_mean_data()
  dataset$type <- "A"
  second <- dataset
  second$type <- "B"
  second$price <- second$price * 2
  dataset <- rbind(dataset, second)
  result <- calculate_spar_index(dataset, c("median", "mean"), "period", "price", grouping_variables = "type")
  expect_named(result, c("median", "mean"))
  expect_equal(result$mean$Index, rep(c(100, 250, 400), 2))
  scaled <- calculate_spar_index(dataset, "mean", "period", "price",
    grouping_variables = "type", index_type_variable = "type", chained = TRUE, base_value = 200)
  expect_equal(scaled$Index, 2 * result$mean$Index)
  expect_error(calculate_spar_index(dataset, c("spar_arithmetic", "mean"), "period", "price",
    grouping_variables = "type"), "appraisal_variable")
})

test_that("all five methods combine, plot, and run identically in parallel", {
  data("arithmetic_data", package = "REPS")
  args <- list(dataset = arithmetic_data,
    method = c("spar_arithmetic", "spar_geometric", "spar_unweighted", "median", "mean"),
    period_variable = "period", dependent_variable = "price", appraisal_variable = "appraisal_value",
    grouping_variables = c("appraisal_year", "property_type"), index_type_variable = "property_type", chained = TRUE)
  sequential <- do.call(calculate_spar_index, args)
  expect_named(sequential, args$method)
  expect_equal(do.call(calculate_spar_index, c(args, list(parallel = TRUE))), sequential)
  for (method in args$method) {
    single_args <- args
    single_args$method <- method
    expect_equal(sequential[[method]], do.call(calculate_spar_index, single_args))
  }
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot_price_index(lapply(sequential, function(x) x[x$property_type == "Apartment", ])))
})

test_that("mean contributions match manual recalculation and handle unavailable exclusions", {
  dataset <- data.frame(period = rep(c(202101, 202102), each = 3), price = c(10, 20, 90, 20, 40, 200))
  result <- calculate_spar_index(dataset, "mean", "period", "price", index_contribution = TRUE)
  expect_named(result, c("Index", "Index_contribution"))
  expect_equal(result$Index$Index, c(100, 650 / 3))
  row <- result$Index_contribution[result$Index_contribution$row_id == "4", ]
  expect_equal(row$Index_excl_observation, 300)
  expect_equal(row$Index_difference, -250 / 3)
  expect_equal(row$PoP_excl_observation, 200)
  unavailable <- data.frame(period = c(202101, 202101), price = c(10, NA_real_))
  excluded <- calculate_spar_index(unavailable, "mean", "period", "price", index_contribution = TRUE)
  expect_true(is.na(excluded$Index_contribution$Index_excl_observation[excluded$Index_contribution$row_id == "1"]))
  expect_error(calculate_spar_index(dataset, c("mean", "median"), "period", "price",
    index_contribution = TRUE), "single method")
})

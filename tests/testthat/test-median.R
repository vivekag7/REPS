median_test_data <- function() {
  data.frame(
    quarter = c(rep("2020Q1", 3), rep("2020Q2", 4), rep("2021Q1", 3)),
    sale_price = c(10, 20, 90, 20, 30, 50, 200, 40, 80, 400)
  )
}

median_test_index <- function(dataset = median_test_data(), ...) {
  calculate_hedonic_index(
    dataset = dataset,
    method = "median",
    period_variable = "quarter",
    dependent_variable = "sale_price",
    ...
  )
}

test_that("median uses untransformed prices and counts complete observations", {
  # Odd/even samples have medians 20, 40, 80, including large outliers.
  result <- median_test_index()
  expect_equal(result, data.frame(
    period = c("2020Q1", "2020Q2", "2021Q1"),
    number_of_observations = c(3L, 4L, 3L),
    Index = c(100, 200, 400)
  ))
  without_counts <- median_test_index(number_of_observations = FALSE)
  expect_equal(without_counts, result[c("period", "Index")])
  expect_equal(median_test_index(number_of_observations = TRUE), result)
  shuffled <- median_test_data()[c(10:1), ]
  expect_equal(median_test_index(shuffled), result)
})

test_that("median shares exact and prefix reference-period conventions", {
  expect_equal(
    median_test_index(reference_period = "2020Q2")$Index,
    c(50, 100, 200)
  )
  # Reference-year denominator is mean(c(20, 40)), not a pooled median.
  expect_equal(
    median_test_index(reference_period = 2020)$Index,
    c(20, 40, 80) / 30 * 100
  )
  expect_error(median_test_index(reference_period = "2019"), "reference period")
})

test_that("median requires no predictors and ignores their missing values", {
  dataset <- median_test_data()
  dataset$area <- NA_real_
  dataset$type <- NA_character_
  expected <- median_test_index()
  expect_equal(median_test_index(dataset), expected)
  expect_equal(median_test_index(
    dataset,
    numerical_variables = "area",
    categorical_variables = "type"
  ), expected)
})

test_that("median uses only complete price-period pairs", {
  dataset <- rbind(median_test_data(), data.frame(
    quarter = c("2020Q1", NA, "", "2022Q1"),
    sale_price = c(NA, 1000, 1000, NA)
  ))
  result <- chained <- NULL
  expect_warning(result <- median_test_index(dataset), "period variable")
  expect_warning(chained <- median_test_index(dataset, chained = TRUE), "period variable")
  expect_equal(result, median_test_index())
  expect_equal(chained, median_test_index())
  expect_error(median_test_index(dataset[FALSE, ]), "complete price-period")
  dataset$sale_price <- NA_real_
  expect_warning(expect_error(median_test_index(dataset), "complete price-period"), "period variable")
  expect_warning(expect_error(
    median_test_index(dataset, chained = TRUE), "complete price-period"
  ), "period variable")
})

test_that("median registration and validation preserve hedonic requirements", {
  expect_true("median" %in% supported_hedonic_index_methods())
  expect_equal(hedonic_index_method_functions()[["median"]], "calculate_median")
  expect_identical(get_hedonic_index_method_function("median"), calculate_median)
  expect_equal(validate_hedonic_index_methods("MEDIAN"), "median")
  expect_error(validate_hedonic_index_methods("invalid"), "Invalid method")
  expect_error(get_hedonic_index_method_function("invalid"), "Invalid method")

  for (methods in list("fisher", c("median", "fisher"), c("fisher", "median"))) {
    expect_error(calculate_hedonic_index(
      dataset = median_test_data(), method = methods,
      period_variable = "quarter", dependent_variable = "sale_price"
    ), "explanatory variable")
  }

  non_numeric <- median_test_data()
  non_numeric$sale_price <- as.character(non_numeric$sale_price)
  expect_error(median_test_index(non_numeric), "not numeric")
  expect_error(median_test_index(median_test_data()["quarter"]), "missing")
})

test_that("median and Fisher share the multi-method runner without changing Fisher", {
  dataset <- data.frame(
    quarter = rep(c("2020Q1", "2020Q2", "2021Q1"), each = 4),
    sale_price = rep(c(100, 150, 200, 300), 3) * rep(c(1, 1.2, 1.5), each = 4),
    area = rep(c(10, 20, 30, 40), 3)
  )
  args <- list(
    dataset = dataset, period_variable = "quarter",
    dependent_variable = "sale_price", numerical_variables = "area",
    categorical_variables = NULL,
    reference_period = "2020Q1", number_of_observations = FALSE
  )
  result <- do.call(calculate_hedonic_index, c(args, list(method = c("median", "fisher"))))
  expect_named(result, c("median", "fisher"))
  expect_equal(result$median$Index, c(100, 120, 150))
  expect_equal(result$fisher, do.call(calculate_fisher, args))
  expect_equal(result$fisher$Index, c(100, 120, 150), tolerance = 1e-8)
  expect_equal(result$median, median_test_index(dataset, number_of_observations = FALSE))
})

test_that("median uses generic annual-overlap chaining and retains counts", {
  dataset <- data.frame(
    quarter = rep(c("2020Q1", "2020Q4", "2021Q1", "2021Q4", "2022Q1"), each = 2),
    sale_price = rep(c(20, 30, 45, 60, 90), each = 2)
  )
  result <- median_test_index(dataset, chained = TRUE)
  expect_equal(result$Index, c(100, 150, 225, 300, 450))
  expect_equal(result$number_of_observations, rep(2L, 5))
  expect_equal(result, median_test_index(dataset))
  expect_equal(
    median_test_index(dataset, chained = TRUE, reference_period = "2021")$Index,
    c(20, 30, 45, 60, 90) / mean(c(45, 60)) * 100
  )
  expect_equal(
    median_test_index(dataset, chained = TRUE, number_of_observations = FALSE),
    result[c("period", "Index")]
  )
})

test_that("median supports generic bootstrap margins without predictors, including chaining", {
  dataset <- median_test_data()
  for (chained in c(FALSE, TRUE)) {
    original <- median_test_index(dataset, chained = chained)
    set.seed(76)
    result <- median_test_index(dataset, chained = chained, margins = TRUE,
      bootstrap_replicates = 5)
    expect_identical(result[names(original)], original)
    expect_true(all(is.finite(result$bootstrap_variance) & result$bootstrap_variance >= 0))
    expect_true(all(result$lower_margin <= result$Index & result$Index <= result$upper_margin))
  }
})

test_that("median calculation returns a standardized unadjusted index", {
  tiny_data <- data.frame(
    period = c("2020Q1", "2020Q1", "2020Q1", "2020Q2", "2020Q2", "2020Q2"),
    price = c(100, 200, 300, 120, 240, 360)
  )

  result <- calculate_median(
    dataset = tiny_data,
    period_variable = "period",
    dependent_variable = "price",
    reference_period = "2020Q1",
    number_of_observations = FALSE
  )

  expect_equal(
    result,
    data.frame(
      period = c("2020Q1", "2020Q2"),
      Index = c(100, 120)
    )
  )
})


test_that("median can be plotted alongside hedonic methods", {
  result <- calculate_hedonic_index(
    method = c("fisher", "paasche", "median"),
    dataset = hedonic_data,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = c("floor_area", "dist_trainstation"),
    categorical_variables = c("neighbourhood_code", "dummy_large_city"),
    reference_period = "2015",
    number_of_observations = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("fisher", "paasche", "median"))
  expect_equal(names(result$median), c("period", "Index"))
})

test_that("Test plot_price_index", {

  # 1. Normal plotting should work silently
  result_normal <- calculate_hedonic_index(
    method = c("fisher", "paasche"),
    dataset = hedonic_data,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015
  )

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_silent(plot_price_index(result_normal))

  # 2. Plotting HMTS resting_points list should trigger custom error
  result_resting <- calculate_hedonic_index(
    method = "hmts",
    dataset = hedonic_data,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015,
    number_preliminary_periods = 2,
    production_since = NULL,
    resting_points = TRUE
  )

  expect_error(
    plot_price_index(result_resting),
    "valid multi-method output"
  )
})


test_that("plot_price_index supports complete and partial confidence bands", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  df <- data.frame(period = c("2024Q1", "2024Q2", "2024Q3", "2024Q4"),
    Index = c(100, 103, 105, 109),
    lower_margin = c(96, 99, NA, 102),
    upper_margin = c(104, 107, NA, 116))
  plain <- df[c("period", "Index")]
  expect_silent(plot_price_index(df))
  expect_silent(plot_price_index(list(fisher = df, median = plain)))
  expect_silent(plot_price_index(list(fisher = plain, median = df)))
  df$lower_margin <- c(96, 99, 101, 102)
  df$upper_margin <- c(104, 107, 110, 116)
  expect_silent(plot_price_index(df))
  df$lower_margin <- rep(NA_real_, nrow(df))
  expect_silent(plot_price_index(df))
  df$lower_margin <- "invalid"
  expect_error(plot_price_index(df), "must be numeric")
})

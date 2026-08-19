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

  # 3. Chained SPAR multi-method output can be plotted after selecting one
  # property-type series.
  data("aritmethic_data", package = "REPS")
  spar_data <- aritmethic_data[
    aritmethic_data[["Property Type"]] == "A",
  ]
  spar_result <- calculate_spar(
    dataset = spar_data,
    method = c("arithmetic", "geometric", "unweighted"),
    period_variable = "Period",
    dependent_variable = "Price",
    appraisal_variable = "Appraisal Value",
    grouping_variables = c("Appraisal Year", "Property Type"),
    index_type_variable = "Property Type",
    chained = TRUE
  )

  expect_silent(
    plot_price_index(spar_result, title = "SPAR Method Comparison")
  )

  expect_error(
    plot_price_index(calculate_spar(
      dataset = aritmethic_data,
      method = "arithmetic",
      period_variable = "Period",
      dependent_variable = "Price",
      appraisal_variable = "Appraisal Value",
      grouping_variables = c("Appraisal Year", "Property Type"),
      index_type_variable = "Property Type",
      chained = TRUE
    )),
    "multiple rows for the same period"
  )
})

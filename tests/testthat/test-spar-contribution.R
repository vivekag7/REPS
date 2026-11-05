spar_contribution_data <- function() {
  data.frame(
    month = rep(rep(c(202101, 202112, 202201, 202202), each = 3), 2),
    vintage = rep(rep(c(2019, 2019, 2020, 2020), each = 3), 2),
    type = rep(c("A", "B"), each = 12),
    price = rep(c(100, 150, 250), 8) * rep(c(1, 1.2, 3, 3.3, 1, 1.5, 4, 3.2), each = 3),
    appraisal = rep(c(80, 120, 220), 8),
    unit = rep(c("u1", "u2", "u3"), 8)
  )
}

spar_contribution_run <- function(dataset = spar_contribution_data(), method = "spar_arithmetic", ...) {
  args <- list(dataset = dataset, method = method, period_variable = "month", dependent_variable = "price",
    appraisal_variable = "appraisal", grouping_variables = c("vintage", "type"),
    index_type_variable = "type", chained = TRUE)
  do.call(calculate_spar_index, utils::modifyList(args, list(...), keep.null = TRUE))
}

test_that("SPAR row contributions match manual exclusion in each independent series", {
  dataset <- spar_contribution_data()
  for (method in grep("^spar_", supported_spar_index_methods(), value = TRUE)) {
    for (chained in c(FALSE, TRUE)) {
      result <- spar_contribution_run(method = method, chained = chained, index_contribution = TRUE)
      expect_named(result, c("Index", "Index_contribution"))
      expect_equal(result$Index, spar_contribution_run(method = method, chained = chained))
      contribution <- result$Index_contribution
      expect_equal(nrow(contribution), 6L)
      expect_true(all(c("vintage", "type", "row_id", "PoP_difference") %in% names(contribution)))
      row <- contribution[contribution$type == "A", ][1, ]
      manual <- spar_contribution_run(dataset[-as.integer(row$row_id), ], method, chained = chained)
      manual <- manual[manual$type == "A", ]
      if (!chained) manual <- manual[manual$vintage == 2020, ]
      manual <- manual[order(manual$period), ]
      values <- manual$Index[manual$period == "202202"]
      pop <- 100 * (values / manual$Index[manual$period == "202201"] - 1)
      expect_equal(row$Index_excl_observation, values)
      expect_equal(row$PoP_excl_observation, pop)
      expect_equal(row$Index_difference, row$Index_original - values)
      expect_equal(row$PoP_difference, pop - row$PoP_original)
    }
  }
})

test_that("SPAR unit contributions remove units only within the analyzed series and period", {
  dataset <- spar_contribution_data()
  dataset$unit <- rep(c("u1", "u1", "u3"), 8)
  result <- spar_contribution_run(dataset, index_contribution = TRUE, unit_variable = "unit",
    index_contribution_period = 202201)
  expect_equal(nrow(result$Index_contribution), 4L)
  row <- result$Index_contribution[result$Index_contribution$type == "A" &
    result$Index_contribution$unit == "u1", ]
  manual <- spar_contribution_run(dataset[!(dataset$month == 202201 & dataset$type == "A" & dataset$unit == "u1"), ])
  expect_equal(row$Index_excl_observation, manual$Index[manual$type == "A" & manual$period == "202201"])
  sequential <- spar_contribution_run(dataset, index_contribution = TRUE, unit_variable = "unit")
  parallel <- spar_contribution_run(dataset, index_contribution = TRUE, unit_variable = "unit", parallel = TRUE)
  expect_equal(parallel, sequential)
})

test_that("removing all target observations produces missing contribution values", {
  dataset <- spar_contribution_data()
  dataset$unit <- "all"
  result <- spar_contribution_run(dataset, index_contribution = TRUE, unit_variable = "unit")
  expect_equal(nrow(result$Index_contribution), 2L)
  expect_true(all(is.na(result$Index_contribution$Index_excl_observation)))
  expect_true(all(is.na(result$Index_contribution$PoP_excl_observation)))
})

test_that("SPAR contribution options reject ambiguous or invalid inputs", {
  expect_error(spar_contribution_run(method = c("spar_arithmetic", "spar_geometric"),
    index_contribution = TRUE), "single method")
  expect_error(spar_contribution_run(index_contribution = NA), "index_contribution")
  expect_error(spar_contribution_run(index_contribution = TRUE, unit_variable = "absent"), "unit_variable")
  expect_error(spar_contribution_run(index_contribution = TRUE, index_contribution_period = c(202201,202202)), "one period")
  expect_error(spar_contribution_run(index_contribution = TRUE, index_contribution_period = "200001"), "not present")
  dataset <- spar_contribution_data()
  dataset$unit[dataset$month == 202202] <- NA_character_
  expect_error(spar_contribution_run(dataset, index_contribution = TRUE, unit_variable = "unit"), "identifiers")
})

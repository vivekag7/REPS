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

spar_contribution_run <- function(dataset = spar_contribution_data(), method = "arithmetic", ...) {
  args <- list(dataset = dataset, method = method, period_variable = "month", dependent_variable = "price",
    appraisal_variable = "appraisal", grouping_variables = c("vintage", "type"),
    index_type_variable = "type", chained = TRUE)
  do.call(calculate_spar_index, utils::modifyList(args, list(...), keep.null = TRUE))
}

test_that("SPAR row contributions match manual exclusion in each independent series", {
  dataset <- spar_contribution_data()
  for (method in supported_spar_index_methods()) {
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
  expect_error(spar_contribution_run(method = c("arithmetic", "geometric"),
    index_contribution = TRUE), "single method")
  expect_error(spar_contribution_run(index_contribution = NA), "index_contribution")
  expect_error(spar_contribution_run(index_contribution = TRUE, unit_variable = "absent"), "unit_variable")
  expect_error(spar_contribution_run(index_contribution = TRUE, index_contribution_period = c(202201,202202)), "one period")
  expect_error(spar_contribution_run(index_contribution = TRUE, index_contribution_period = "200001"), "not present")
  dataset <- spar_contribution_data()
  dataset$unit[dataset$month == 202202] <- NA_character_
  expect_error(spar_contribution_run(dataset, index_contribution = TRUE, unit_variable = "unit"), "identifiers")
})

test_that("SPAR margins preserve the original contribution analysis", {
  original <- spar_contribution_run(index_contribution = TRUE, unit_variable = "unit")
  set.seed(85)
  result <- spar_contribution_run(index_contribution = TRUE, unit_variable = "unit",
    margins = TRUE, bootstrap_replicates = 3, bootstrap_strata = c("month", "vintage", "type"))
  expect_named(result, c("Index", "Index_contribution"))
  expect_identical(result$Index_contribution, original$Index_contribution)
  expect_identical(result$Index[names(original$Index)], original$Index)
  expect_true(all(is.finite(result$Index$bootstrap_variance)))
})

test_that("SPAR recalculation errors use the generic per-unit warning and NA handling", {
  dataset <- spar_contribution_data()
  dataset <- dataset[dataset$type == "A", ]
  dataset$price[dataset$month == 202202 & dataset$unit != "u1"] <- NA_real_
  fields <- c("Index_excl_observation", "Index_difference",
              "PoP_excl_observation", "PoP_difference")
  for (method in supported_spar_index_methods()) {
    original <- spar_contribution_run(dataset, method)
    without_u1 <- dataset[!(dataset$month == 202202 & dataset$unit == "u1"), ]
    # This is a real method failure, not a synthetic callback or an empty period.
    error <- tryCatch(spar_contribution_run(without_u1, method), error = identity)
    expect_s3_class(error, "error")
    expect_match(conditionMessage(error), "Each period/group must have enough observed values",
                 fixed = TRUE)

    warnings <- character()
    result <- withCallingHandlers(spar_contribution_run(
      dataset, method, unit_variable = "unit", index_contribution = TRUE
    ), warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    })
    expect_equal(result$Index, original)
    contribution <- result$Index_contribution
    expect_equal(nrow(contribution), 3L)
    expect_setequal(contribution$unit, c("u1", "u2", "u3"))
    failed <- contribution$unit == "u1"
    expect_true(all(is.na(contribution[failed, fields])))
    expect_true(all(is.finite(as.matrix(contribution[!failed, fields]))))
    original_level <- original$Index[original$period == "202202"]
    original_pop <- 100 * (original_level / original$Index[original$period == "202201"] - 1)
    expect_equal(contribution$Index_original, rep(original_level, 3))
    expect_equal(contribution$PoP_original, rep(original_pop, 3))
    expect_length(warnings, 1L)
    expect_match(warnings, "unit/row 'u1'", fixed = TRUE)
    expect_match(warnings, "202202", fixed = TRUE)
    expect_match(warnings, conditionMessage(error), fixed = TRUE)

    for (unit in c("u2", "u3")) {
      manual <- spar_contribution_run(
        dataset[!(dataset$month == 202202 & dataset$unit == unit), ], method
      )
      level <- manual$Index[manual$period == "202202"]
      pop <- 100 * (level / manual$Index[manual$period == "202201"] - 1)
      row <- contribution[contribution$unit == unit, ]
      expect_equal(row$Index_excl_observation, level)
      expect_equal(row$Index_difference, original_level - level)
      expect_equal(row$PoP_excl_observation, pop)
      expect_equal(row$PoP_difference, pop - original_pop)
    }
  }
})

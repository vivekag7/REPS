test_that("calculate_index_contribution returns compact row-level output by default", {
  dataset <- data.frame(
    period = c("2020Q1", "2020Q1", "2020Q2", "2020Q2"),
    price = c(100, 110, 120, 130),
    area = c(50, 60, 50, 60),
    stringsAsFactors = FALSE
  )
  
  original_index <- data.frame(
    period = c("2020Q1", "2020Q2"),
    Index = c(100, 120)
  )
  
  result <- calculate_index_contribution(
    dataset = dataset,
    index_output = original_index,
    period_variable = "period",
    calculate_index_function = function(target_dataset) {
      data.frame(
        period = c("2020Q1", "2020Q2"),
        Index = c(100, 115 + nrow(target_dataset[target_dataset$period == "2020Q2", ]))
      )
    },
    index_contribution_period = "2020Q2"
  )
  
  expect_equal(nrow(result), 2)
  
  expect_named(
    result,
    c(
      "row_id",
      "period",
      "Index_excl_observation",
      "Index_original",
      "Index_difference",
      "PoP_excl_observation",
      "PoP_original",
      "PoP_difference"
    )
  )
  
  expect_equal(result$period, c("2020Q2", "2020Q2"))
  expect_true(all(result$row_id %in% c("3", "4")))
  
  expect_false("price" %in% names(result))
  expect_false("area" %in% names(result))
  expect_false(".index_contribution_unit_id" %in% names(result))
})

test_that("calculate_index_contribution validates regular index output", {
  expect_error(
    calculate_index_contribution(
      dataset = data.frame(period = "2020Q1", price = 100),
      index_output = list(Index = data.frame(period = "2020Q1", Index = 100)),
      period_variable = "period",
      calculate_index_function = function(target_dataset) target_dataset
    ),
    "regular index data.frame"
  )
})

test_that("index contribution PoP values are percentage changes, not index levels", {
  result <- add_period_growth_to_index(
    data.frame(
      period = c("2020Q1", "2020Q2", "2020Q3"),
      Index = c(100, 120, 114)
    )
  )

  expect_equal(result$period_growth, c(0, 20, -5))
})

test_that("parallel core resolution is capped and validated", {
  expect_equal(resolve_parallel_cores(FALSE, n_tasks = 10, n_cores = 4), 1L)
  expect_equal(resolve_parallel_cores(TRUE, n_tasks = 2, n_cores = 4), 2L)
  expect_equal(resolve_parallel_cores(TRUE, n_tasks = 2, n_cores = 1), 1L)

  expect_error(
    resolve_parallel_cores(TRUE, n_tasks = 2, n_cores = 0),
    "positive number"
  )
})

test_that("parallel index contribution path can be requested with one worker", {
  dataset <- data.frame(
    period = c("2020Q1", "2020Q1", "2020Q2", "2020Q2"),
    price = c(100, 110, 120, 130),
    area = c(50, 60, 50, 60),
    stringsAsFactors = FALSE
  )

  original_index <- data.frame(
    period = c("2020Q1", "2020Q2"),
    Index = c(100, 120)
  )

  calculate_index <- function(target_dataset) {
    data.frame(
      period = c("2020Q1", "2020Q2"),
      Index = c(100, 115 + nrow(target_dataset[target_dataset$period == "2020Q2", ]))
    )
  }

  sequential_result <- calculate_index_contribution(
    dataset = dataset,
    index_output = original_index,
    period_variable = "period",
    calculate_index_function = calculate_index,
    index_contribution_period = "2020Q2"
  )

  parallel_requested_result <- calculate_index_contribution(
    dataset = dataset,
    index_output = original_index,
    period_variable = "period",
    calculate_index_function = calculate_index,
    index_contribution_period = "2020Q2",
    parallel = TRUE
  )

  expect_equal(parallel_requested_result, sequential_result)
})

test_that("index contribution progress reports completed runs", {
  progress_output <- capture.output(
    update_index_contribution_progress(
      current_run = 2,
      total_runs = 2,
      progress_enabled = TRUE
    ),
    type = "message"
  )

  expect_true(any(grepl("2/2 runs completed", progress_output, fixed = TRUE)))
})

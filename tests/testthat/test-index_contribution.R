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

contribution_subset_fixture <- function() {
  dataset <- data.frame(
    period = rep(c("2020Q1", "2020Q2"), c(2, 4)),
    price = c(90, 110, 120, 140, 160, 180),
    unit = c("old-A", "old-B", "A", "A", "B", "C")
  )
  callback <- function(data) {
    target <- data[data$period == "2020Q2", ]
    if (!"B" %in% target$unit) {
      stop("Synthetic factor level disappeared; process ", Sys.getpid())
    }
    data.frame(period = c("2020Q1", "2020Q2"), Index = c(100, mean(target$price)))
  }
  list(dataset = dataset, callback = callback, index = callback(dataset))
}

capture_contribution_warnings <- function(code) {
  messages <- character()
  result <- withCallingHandlers(force(code), warning = function(warning) {
    messages <<- c(messages, conditionMessage(warning))
    invokeRestart("muffleWarning")
  })
  list(result = result, warnings = messages)
}

test_that("a failed row or grouped exclusion retains originals and other results", {
  fixture <- contribution_subset_fixture()
  fields <- c("Index_excl_observation", "Index_difference",
              "PoP_excl_observation", "PoP_difference")
  for (unit_variable in list(NULL, "unit")) {
    captured <- capture_contribution_warnings(calculate_index_contribution(
      fixture$dataset, fixture$index, "period", fixture$callback,
      unit_variable = unit_variable, index_contribution_period = "2020Q2"
    ))
    result <- captured$result
    id_column <- if (is.null(unit_variable)) "row_id" else "unit"
    failed_id <- if (is.null(unit_variable)) "5" else "B"
    failed <- result[[id_column]] == failed_id
    expect_equal(nrow(result), if (is.null(unit_variable)) 4L else 3L)
    expect_equal(sum(failed), 1L)
    expect_true(all(is.na(result[failed, fields])))
    expect_true(all(is.finite(as.matrix(result[!failed, fields]))))
    expect_equal(result$Index_original, rep(150, nrow(result)))
    expect_equal(result$PoP_original, rep(50, nrow(result)))
    expect_equal(result$period, rep("2020Q2", nrow(result)))
    expect_length(captured$warnings, 1L)
    expect_match(captured$warnings, paste0("unit/row '", failed_id, "'"), fixed = TRUE)
    expect_match(captured$warnings, "2020Q2", fixed = TRUE)
    expect_match(captured$warnings, "Synthetic factor level disappeared", fixed = TRUE)
    for (id in result[[id_column]][!failed]) {
      keep <- if (is.null(unit_variable)) row.names(fixture$dataset) != id else
        !(fixture$dataset$period == "2020Q2" & fixture$dataset$unit == id)
      manual <- fixture$callback(fixture$dataset[keep, ])$Index[2]
      row <- result[result[[id_column]] == id, ]
      expect_equal(row$Index_excl_observation, manual)
      expect_equal(row$Index_difference, 150 - manual)
      expect_equal(row$PoP_excl_observation, manual - 100)
      expect_equal(row$PoP_difference, manual - 150)
    }
  }
})

test_that("actual parallel workers return failed-exclusion warnings to the parent", {
  # Batch the tiny fixture and force two workers to exercise socket/fork transport.
  local_mocked_bindings(
    get_index_contribution_chunk_size = function(total_runs, max_updates = 100L) total_runs,
    resolve_parallel_cores = function(parallel, n_tasks, n_cores = NULL) {
      if (isTRUE(parallel) && n_tasks > 1L) 2L else 1L
    }
  )
  fixture <- contribution_subset_fixture()
  run <- function(parallel) capture_contribution_warnings(calculate_index_contribution(
    fixture$dataset, fixture$index, "period", fixture$callback,
    unit_variable = "unit", index_contribution_period = "2020Q2", parallel = parallel
  ))
  sequential <- run(FALSE)
  parallel <- run(TRUE)
  expect_equal(parallel$result, sequential$result)
  expect_length(parallel$warnings, 1L)
  expect_match(parallel$warnings, "unit/row 'B'", fixed = TRUE)
  expect_match(parallel$warnings, "2020Q2", fixed = TRUE)
  expect_match(parallel$warnings, "Synthetic factor level disappeared", fixed = TRUE)
  expect_false(grepl(paste0("process ", Sys.getpid(), "$"), parallel$warnings))
  expect_match(sequential$warnings, paste0("process ", Sys.getpid(), "$"))
})

test_that("global contribution validation errors remain errors", {
  fixture <- contribution_subset_fixture()
  expect_error(calculate_index_contribution(
    fixture$dataset, fixture$index, "missing", fixture$callback
  ), "missing the period variable")
  expect_error(calculate_index_contribution(
    fixture$dataset, fixture$index, "period", fixture$callback, unit_variable = "missing"
  ), "missing the provided")
  expect_error(calculate_index_contribution(
    fixture$dataset, fixture$index, "period", fixture$callback,
    index_contribution_period = "1900Q1"
  ), "must match exactly one period")
  expect_error(calculate_index_contribution(
    fixture$dataset, data.frame(period = "2020Q2"), "period", fixture$callback
  ), "regular index data.frame")
})

test_that("invalid callback output is outside the recalculation exception boundary", {
  fixture <- contribution_subset_fixture()
  expect_error(calculate_index_contribution(
    fixture$dataset, fixture$index, "period",
    function(data) data.frame(period = "2020Q2"), unit_variable = "unit"
  ), "regular index data.frame")
})

test_that("an original hedonic index failure is not converted to contribution rows", {
  local_mocked_bindings(
    run_hedonic_index_method = function(...) stop("Original index fit failed")
  )
  fixture <- contribution_subset_fixture()
  fixture$dataset$area <- seq_len(nrow(fixture$dataset))
  expect_error(calculate_hedonic_index(
    dataset = fixture$dataset, method = "timedummy",
    period_variable = "period", dependent_variable = "price",
    numerical_variables = "area", categorical_variables = NULL,
    index_contribution = TRUE, index_contribution_period = "2020Q2"
  ), "Original index fit failed")
})

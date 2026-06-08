test_that("Test calculate_hedonic_index", {
  
  # 1. Chained Fisher reference output should match saved reference
  save_refs <- FALSE
  ref_file <- test_path("test_data", "chained_fisher_output.rds")
  
  tbl_chained <- calculate_hedonic_index(
    method = "fisher",
    chained = TRUE,
    dataset = hedonic_data,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015
  )
  
  if (save_refs) {
    dir.create(dirname(ref_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tbl_chained, ref_file)
    succeed("Reference file saved.")
  } else {
    ref_tbl <- readRDS(ref_file)
    expect_equal(tbl_chained, ref_tbl, tolerance = 1e-8)
  }
  
  # 2. Invalid method should error
  expect_error(
    calculate_hedonic_index(
      method = "invalid",
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code"
    ),
    "Invalid method"
  )
  
  # 3. Single method should work
  expect_silent(
    calculate_hedonic_index(
      method = "fisher",
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015
    )
  )
  
  # 4. Multiple methods without HMTS should work
  result <- calculate_hedonic_index(
    method = c("fisher", "paasche", "timedummy"),
    dataset = hedonic_data,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015
  )
  
  expect_type(result, "list")
  expect_named(result, c("fisher", "paasche", "timedummy"))
  
  # 5. Multiple methods with resting_points = TRUE should error
  expect_error(
    calculate_hedonic_index(
      method = c("fisher", "hmts"),
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      periods_in_year = 4,
      number_preliminary_periods = 2,
      resting_points = TRUE
    ),
    "resting_points = TRUE"
  )
  
  # 6. Chained index with resting_points = TRUE should error
  expect_error(
    calculate_hedonic_index(
      method = "hmts",
      chained = TRUE,
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      periods_in_year = 4,
      number_preliminary_periods = 2,
      resting_points = TRUE
    ),
    "chained = TRUE"
  )
  
  # 7. RTD missing window_length triggers message
  expect_message(
    calculate_hedonic_index(
      method = "rolling_timedummy",
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015
    ),
    "A default value of 5 has been applied"
  )
  
  # 8. HMTS missing parameters triggers messages
  hmts_messages <- character()
  withCallingHandlers(
    calculate_hedonic_index(
      method = "hmts",
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015
    ),
    message = function(message) {
      hmts_messages <<- c(hmts_messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(any(grepl("number_preliminary_periods", hmts_messages)))
  expect_true(any(grepl("production since", hmts_messages)))
  
  # 9. Repricing missing periods_in_year triggers error
  expect_error(
    calculate_hedonic_index(
      method = "repricing",
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015
    ),
    "periods_in_year"
  )
})


test_that("hedonic index method validation normalizes and rejects methods", {
  expect_equal(
    validate_hedonic_index_methods(c("FISHER", "Paasche")),
    c("fisher", "paasche")
  )

  expect_error(
    validate_hedonic_index_methods("unknown"),
    "Invalid method"
  )
})


test_that("hedonic index option validation prevents unsupported combinations", {
  expect_error(
    validate_hedonic_index_options(
      method = c("fisher", "hmts"),
      chained = FALSE,
      index_mutation = FALSE,
      extra_args = list(resting_points = TRUE)
    ),
    "resting_points = TRUE"
  )

  expect_error(
    validate_hedonic_index_options(
      method = "hmts",
      chained = TRUE,
      index_mutation = FALSE,
      extra_args = list(resting_points = TRUE)
    ),
    "chained = TRUE"
  )

  expect_error(
    validate_hedonic_index_options(
      method = c("fisher", "paasche"),
      chained = FALSE,
      index_mutation = TRUE,
      extra_args = list()
    ),
    "index_mutation = TRUE"
  )

  expect_error(
    validate_hedonic_index_options(
      method = "hmts",
      chained = FALSE,
      index_mutation = TRUE,
      extra_args = list(resting_points = TRUE)
    ),
    "regular index data.frame"
  )

  expect_invisible(
    validate_hedonic_index_options(
      method = "hmts",
      chained = FALSE,
      index_mutation = FALSE,
      extra_args = list(resting_points = TRUE)
    )
  )
})


test_that("index_mutation output is only available for single-method calculations", {
  expect_error(
    calculate_hedonic_index(
      method = c("fisher", "paasche"),
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      index_mutation = TRUE
    ),
    "index_mutation = TRUE"
  )
})


test_that("parallel hedonic index output matches sequential output", {
  sequential_result <- calculate_hedonic_index(
    method = c("fisher", "paasche"),
    dataset = hedonic_data,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015,
    parallel = FALSE
  )

  parallel_result <- calculate_hedonic_index(
    method = c("fisher", "paasche"),
    dataset = hedonic_data,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "floor_area",
    categorical_variables = "neighbourhood_code",
    reference_period = 2015,
    parallel = TRUE
  )

  expect_equal(parallel_result, sequential_result, tolerance = 1e-8)
})


test_that("index_mutation returns index and contribution tables", {
  tiny_data <- data.frame(
    period = rep(c("2020Q1", "2020Q2", "2020Q3"), each = 4),
    price = c(100, 110, 105, 115, 120, 132, 126, 138, 140, 154, 147, 161),
    area = rep(c(50, 60, 55, 65), 3),
    type = rep(c("A", "B", "A", "B"), 3),
    unit_id = rep(c("u1", "u2", "u3", "u4"), 3),
    stringsAsFactors = FALSE
  )

  result <- calculate_hedonic_index(
    method = "timedummy",
    dataset = tiny_data,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = "area",
    categorical_variables = "type",
    reference_period = "2020Q1",
    number_of_observations = FALSE,
    index_mutation = TRUE,
    unit_variable = "unit_id",
    index_mutation_period = "2020Q3"
  )

  expect_type(result, "list")
  expect_named(result, c("Index", "Index_mutation"))
  expect_equal(names(result$Index), c("period", "Index"))
  expect_equal(nrow(result$Index_mutation), 4)
  expect_true(all(result$Index_mutation$period == "2020Q3"))
  expect_true(all(c(
    "unit_id",
    "period",
    "Index_excl_observation",
    "Index_original",
    "Index_difference",
    "PoP_excl_observation",
    "PoP_original",
    "PoP_difference"
  ) %in% names(result$Index_mutation)))
})


test_that("method extra arguments are filtered and defaulted", {
  rolling_method <- function(window_length = NULL) {}
  rolling_args <- NULL
  expect_message(
    {
      rolling_args <- resolve_hedonic_method_extra_args(
        method = "rolling_timedummy",
        target_function = rolling_method,
        extra_args = list(ignored = TRUE)
      )
    },
    "window_length"
  )

  expect_equal(rolling_args$window_length, 5)
  expect_false("ignored" %in% names(rolling_args))

  hmts_method <- function(number_preliminary_periods = NULL,
                          production_since = NULL,
                          resting_points = NULL) {}

  hmts_args <- NULL
  hmts_messages <- character()
  hmts_args <- withCallingHandlers(
    resolve_hedonic_method_extra_args(
      method = "hmts",
      target_function = hmts_method,
      extra_args = list()
    ),
    message = function(message) {
      hmts_messages <<- c(hmts_messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(any(grepl("number_preliminary_periods", hmts_messages)))
  expect_true(any(grepl("production since", hmts_messages)))
  expect_equal(hmts_args$number_preliminary_periods, 3)
  expect_null(hmts_args$production_since)
  expect_false(hmts_args$resting_points)

  repricing_method <- function(periods_in_year = NULL) {}
  expect_error(
    resolve_hedonic_method_extra_args(
      method = "repricing",
      target_function = repricing_method,
      extra_args = list()
    ),
    "periods_in_year"
  )

  laspeyres_method <- function(imputation = NULL) {}
  laspeyres_args <- resolve_hedonic_method_extra_args(
    method = "laspeyres",
    target_function = laspeyres_method,
    extra_args = list()
  )

  expect_false(laspeyres_args$imputation)
})


test_that("single hedonic index method delegates unchained calculations", {
  dataset <- data.frame(period = "2015Q1", price = 100)
  captured <- NULL

  result <- calculate_single_hedonic_index_method(
    method = "fisher",
    dataset = dataset,
    period_variable = "period",
    reference_period = "2015Q1",
    chained = FALSE,
    run_method = function(method, target_dataset, target_reference_period) {
      captured <<- list(
        method = method,
        periods = target_dataset$period,
        reference_period = target_reference_period
      )
      data.frame(period = target_dataset$period, Index = 100)
    }
  )

  expect_equal(result, data.frame(period = "2015Q1", Index = 100))
  expect_equal(captured$method, "fisher")
  expect_equal(captured$periods, "2015Q1")
  expect_equal(captured$reference_period, "2015Q1")
})


test_that("annual overlap chaining builds segments and splices growth factors", {
  dataset <- data.frame(
    period = c("2015Q1", "2015Q2", "2016Q1", "2016Q2"),
    price = c(100, 110, 130, 140)
  )
  calls <- list()

  result <- calculate_chained_hedonic_index(
    dataset = dataset,
    period_variable = "period",
    reference_period = NULL,
    run_method = function(target_dataset, target_reference_period) {
      periods <- sort(unique(as.character(target_dataset$period)))
      calls[[length(calls) + 1]] <<- list(
        periods = periods,
        reference_period = target_reference_period
      )

      index_values <- if (is.null(target_reference_period)) {
        c(100, 110)
      } else {
        c(100, 120, 130)
      }

      data.frame(period = periods, Index = index_values)
    }
  )

  expect_equal(length(calls), 2)
  expect_equal(calls[[1]]$periods, c("2015Q1", "2015Q2"))
  expect_null(calls[[1]]$reference_period)
  expect_equal(calls[[2]]$periods, c("2015Q2", "2016Q1", "2016Q2"))
  expect_equal(calls[[2]]$reference_period, "2015Q2")

  expect_equal(
    result,
    data.frame(
      period = c("2015Q1", "2015Q2", "2016Q1", "2016Q2"),
      Index = c(100, 110, 132, 143)
    )
  )
})


test_that("chained index helpers validate period and method result shape", {
  expect_error(
    calculate_chained_hedonic_index(
      dataset = data.frame(period = c("bad1", "bad2")),
      period_variable = "period",
      reference_period = NULL,
      run_method = function(target_dataset, target_reference_period) {
        data.frame(period = target_dataset$period, Index = 100)
      }
    ),
    "first four characters"
  )

  expect_error(
    validate_chained_index_result(data.frame(period = "2015Q1", value = 100)),
    "columns 'period' and 'Index'"
  )
})


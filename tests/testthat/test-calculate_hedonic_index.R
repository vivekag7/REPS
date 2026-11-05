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
      index_contribution = FALSE,
      extra_args = list(resting_points = TRUE)
    ),
    "resting_points = TRUE"
  )

  expect_error(
    validate_hedonic_index_options(
      method = "hmts",
      chained = TRUE,
      index_contribution = FALSE,
      extra_args = list(resting_points = TRUE)
    ),
    "chained = TRUE"
  )

  expect_error(
    validate_hedonic_index_options(
      method = c("fisher", "paasche"),
      chained = FALSE,
      index_contribution = TRUE,
      extra_args = list()
    ),
    "index_contribution = TRUE"
  )

  expect_error(
    validate_hedonic_index_options(
      method = "hmts",
      chained = FALSE,
      index_contribution = TRUE,
      extra_args = list(resting_points = TRUE)
    ),
    "regular index data.frame"
  )

  expect_invisible(
    validate_hedonic_index_options(
      method = "hmts",
      chained = FALSE,
      index_contribution = FALSE,
      extra_args = list(resting_points = TRUE)
    )
  )
})


test_that("index_contribution output is only available for single-method calculations", {
  expect_error(
    calculate_hedonic_index(
      method = c("fisher", "paasche"),
      dataset = hedonic_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = "floor_area",
      categorical_variables = "neighbourhood_code",
      reference_period = 2015,
      index_contribution = TRUE
    ),
    "index_contribution = TRUE"
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


test_that("index_contribution returns index and contribution tables", {
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
    index_contribution = TRUE,
    unit_variable = "unit_id",
    index_contribution_period = "2020Q3"
  )

  expect_type(result, "list")
  expect_named(result, c("Index", "Index_contribution"))
  expect_equal(names(result$Index), c("period", "Index"))
  expect_equal(nrow(result$Index_contribution), 4)
  expect_true(all(result$Index_contribution$period == "2020Q3"))
  expect_true(all(c(
    "unit_id",
    "period",
    "Index_excl_observation",
    "Index_original",
    "Index_difference",
    "PoP_excl_observation",
    "PoP_original",
    "PoP_difference"
  ) %in% names(result$Index_contribution)))
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

test_that("contribution matches manual leave-one-unit recalculation", {
  data("hedonic_data", package = "REPS")
  
  unit_variable <- ".test_contribution_unit"
  
  add_test_contribution_units <- function(dataset, period_variable,
                                          contribution_period,
                                          number_of_units = 4L) {
    dataset[[unit_variable]] <- "outside_target_period"
    
    target_rows <- which(
      as.character(dataset[[period_variable]]) == as.character(contribution_period)
    )
    
    dataset[[unit_variable]][target_rows] <- paste0(
      "test_unit_",
      ((seq_along(target_rows) - 1L) %% number_of_units) + 1L
    )
    
    dataset
  }
  
  suppress_index_contribution_progress <- function(expr) {
    result <- NULL
    invisible(capture.output(result <- force(expr), type = "message"))
    result
  }
  
  periods <- sort(unique(as.character(hedonic_data$period)))
  contribution_period <- tail(periods, 1L)
  
  test_data <- add_test_contribution_units(
    dataset = hedonic_data,
    period_variable = "period",
    contribution_period = contribution_period,
    number_of_units = 4L
  )
  
  selected_unit <- "test_unit_1"
  
  contribution_output <- suppress_index_contribution_progress(
    calculate_hedonic_index(
      method = "fisher",
      dataset = test_data,
      period_variable = "period",
      dependent_variable = "price",
      numerical_variables = c("floor_area", "dist_trainstation"),
      categorical_variables = c("neighbourhood_code", "dummy_large_city"),
      reference_period = "2015",
      number_of_observations = FALSE,
      index_contribution = TRUE,
      index_contribution_period = contribution_period,
      unit_variable = unit_variable,
      parallel = FALSE
    )
  )
  
  original_index_table <- contribution_output$Index
  contribution_table <- contribution_output$Index_contribution
  
  original_index_manual <- original_index_table[
    as.character(original_index_table$period) == contribution_period,
    "Index"
  ]
  
  contribution_row <- contribution_table[
    contribution_table[[unit_variable]] == selected_unit,
    ,
    drop = FALSE
  ]
  
  # Check 1:
  # The contribution table should contain exactly one row for the selected
  # contribution unit.
  expect_equal(
    nrow(contribution_row),
    1L,
    info = "Contribution row should be unique for fisher"
  )
  
  # Check 2:
  # The original index in the contribution table should equal the original
  # index from the regular index output.
  expect_equal(
    contribution_row$Index_original,
    original_index_manual,
    tolerance = 1e-8,
    info = "Original index mismatch for fisher"
  )
  
  manual_dataset_without_unit <- test_data[
    !(
      as.character(test_data$period) == contribution_period &
        test_data[[unit_variable]] == selected_unit
    ),
    ,
    drop = FALSE
  ]
  
  manual_index_without_unit <- calculate_hedonic_index(
    method = "fisher",
    dataset = manual_dataset_without_unit,
    period_variable = "period",
    dependent_variable = "price",
    numerical_variables = c("floor_area", "dist_trainstation"),
    categorical_variables = c("neighbourhood_code", "dummy_large_city"),
    reference_period = "2015",
    number_of_observations = FALSE,
    index_contribution = FALSE,
    unit_variable = unit_variable,
    parallel = FALSE
  )
  
  index_without_unit_manual <- manual_index_without_unit[
    as.character(manual_index_without_unit$period) == contribution_period,
    "Index"
  ]
  
  # Check 3:
  # The index excluding one contribution unit should equal the manually
  # recalculated index excluding the same unit.
  expect_equal(
    contribution_row$Index_excl_observation,
    index_without_unit_manual,
    tolerance = 1e-8,
    info = "Excluded-unit index mismatch for fisher"
  )
})

bootstrap_test_data <- function() {
  data.frame(
    period = rep(c("2020Q1", "2020Q2", "2021Q1"), each = 6),
    price = rep(c(100, 120, 150, 190, 220, 280), 3) * rep(c(1, 1.2, 1.4), each = 6),
    area = rep(c(10, 20, 30, 40, 50, 60), 3)
  )
}

bootstrap_test_run <- function(method = "median", ...) {
  calculate_hedonic_index(
    bootstrap_test_data(), method, "period", "price", numerical_variables = "area", ...
  )
}

capture_bootstrap_warnings <- function(code) {
  messages <- character()
  result <- withCallingHandlers(force(code), warning = function(warning) {
    messages <<- c(messages, conditionMessage(warning))
    invokeRestart("muffleWarning")
  })
  list(result = result, warnings = messages)
}

test_that("disabled bootstrap preserves ordinary output and RNG state", {
  methods <- c("median", "fisher", "timedummy")
  expected <- bootstrap_test_run(methods)
  set.seed(71)
  seed <- .Random.seed
  result <- bootstrap_test_run(methods, margins = FALSE)
  expect_identical(result, expected)
  expect_identical(.Random.seed, seed)
  expect_identical(bootstrap_test_run("median", margins = FALSE,
    bootstrap_replicates = NULL),
    expected$median)
})

test_that("bootstrap margins preserve columns and surround the original index", {
  set.seed(72)
  original <- bootstrap_test_run("timedummy")
  run <- function(...) bootstrap_test_run("timedummy", margins = TRUE,
    bootstrap_replicates = 6, bootstrap_strata = "period", ...)
  result <- run()
  set.seed(72)
  expect_identical(run(confidence_level = 0.95), result)
  set.seed(72)
  narrower <- run(confidence_level = 0.90)
  expect_identical(result[names(original)], original)
  expect_named(result, c(names(original), "bootstrap_variance", "lower_margin", "upper_margin"))
  expect_true(all(is.finite(result$bootstrap_variance) & result$bootstrap_variance >= 0))
  expect_true(any(result$bootstrap_variance > 0))
  expect_true(all(result$lower_margin <= result$Index & result$Index <= result$upper_margin))
  width <- qnorm(0.975) * sqrt(result$bootstrap_variance)
  expect_equal(result$lower_margin, original$Index - width)
  expect_equal(result$upper_margin, original$Index + width)
  expect_identical(narrower$bootstrap_variance, result$bootstrap_variance)
  expect_equal(narrower$lower_margin, original$Index - qnorm(0.95) * sqrt(result$bootstrap_variance))
  expect_equal(narrower$upper_margin, original$Index + qnorm(0.95) * sqrt(result$bootstrap_variance))
  expect_true(all(narrower$lower_margin >= result$lower_margin))
  expect_true(all(narrower$upper_margin <= result$upper_margin))
})

test_that("bootstrap options are validated when margins are enabled", {
  for (bad in list(NULL, NA, 1, c(TRUE, FALSE))) {
    expect_error(bootstrap_test_run(margins = bad), "margins")
  }
  for (bad in list(NULL, NA, Inf, -Inf, 0, 1, -2, 2.5, "5", TRUE, 2i, c(2, 3))) {
    expect_error(bootstrap_test_run(margins = TRUE, bootstrap_replicates = bad), "bootstrap_replicates")
  }
  expect_identical(formals(calculate_hedonic_index)$confidence_level, 0.95)
  expect_null(formals(calculate_hedonic_index)$bootstrap_strata)
  for (bad in list(NULL, NA, NaN, Inf, -Inf, 0, 1, -0.1, 1.1, "0.95", TRUE, 1i, c(0.8, 0.9))) {
    expect_error(bootstrap_test_run(margins = TRUE, confidence_level = bad), "confidence_level")
  }
  for (bad in list(character(), NA_character_, "", " ", 1, c("period", "period"), "missing")) {
    expect_error(bootstrap_test_run(margins = TRUE, bootstrap_strata = bad), "bootstrap_strata")
  }
  expect_error(bootstrap_test_run("hmts", margins = TRUE, resting_points = TRUE), "regular index output")
})

test_that("bootstrap supports full-dataset and single or combined strata sampling", {
  dataset <- data.frame(period = c("a", "a", "a", "b", "b", "b", "b", "c"),
                        region = c("x", "x", "y", "x", "x", "y", NA, "z"), id = 1:8)
  names(dataset)[1] <- "month"
  set.seed(70)
  whole <- sample_bootstrap_rows(dataset)
  set.seed(70)
  expect_equal(whole$id, sample.int(nrow(dataset), nrow(dataset), replace = TRUE))
  set.seed(70)
  expect_identical(sample_bootstrap_rows(dataset, NULL), whole)
  set.seed(73)
  sample <- sample_bootstrap_rows(dataset, "month")
  set.seed(73)
  expected <- c(sample.int(3, 3, replace = TRUE), 3L + sample.int(4, 4, replace = TRUE), 8L)
  expect_equal(sample$id, expected)
  expect_true(anyDuplicated(sample$id) > 0)
  for (i in 1:3) {
    sample <- sample_bootstrap_rows(dataset, "month")
    expect_equal(nrow(sample), nrow(dataset))
    expect_equal(table(sample$month), table(dataset$month))
    expect_equal(sample$id[sample$month == "c"], 8L)
    combined <- sample_bootstrap_rows(dataset, c("month", "region"))
    expect_equal(table(combined[c("month", "region")], useNA = "ifany"),
                 table(dataset[c("month", "region")], useNA = "ifany"))
    expect_equal(combined$id[combined$month == "c"], 8L)
  }
})

test_that("bootstrap values align with period labels including reordered and missing periods", {
  original <- data.frame(period = c("p2", "p1"), Index = c(250, 100), number_of_observations = 2L)
  replicate <- 0L
  result <- calculate_bootstrap_margins(data.frame(x = 1:3), original, 5, "x",
    function(data) {
      replicate <<- replicate + 1L
      output <- data.frame(period = c("p1", "p2"), Index = c(replicate, 10 * replicate))
      if (replicate == 5L) output <- output[2, ]
      output
    }, "synthetic")
  expect_identical(result[names(original)], original)
  expect_equal(result$bootstrap_variance, c(var(seq(10, 50, 10)), var(1:4)))
  expect_equal((result$lower_margin + result$upper_margin) / 2, original$Index)
})

test_that("one failed bootstrap replicate warns and leaves remaining replicates usable", {
  replicate <- 0L
  captured <- capture_bootstrap_warnings(calculate_bootstrap_margins(
    data.frame(x = 1:3), data.frame(period = "p1", Index = 120), 5, "x",
    function(data) {
      replicate <<- replicate + 1L
      if (replicate == 3L) stop("Synthetic model fit failed")
      data.frame(period = "p1", Index = 100 + replicate)
    }, "synthetic"
  ))
  expect_equal(replicate, 5L)
  expect_length(captured$warnings, 1L)
  expect_match(captured$warnings, "replicate 3.*synthetic.*Synthetic model fit failed")
  expect_equal(captured$result$bootstrap_variance, var(c(101, 102, 104, 105)))
  expect_true(is.finite(captured$result$lower_margin))
})

test_that("insufficient valid bootstrap values warn and produce NA margins per period", {
  replicate <- 0L
  original <- data.frame(period = c("p1", "p2"), Index = c(100, 120))
  captured <- capture_bootstrap_warnings(calculate_bootstrap_margins(
    data.frame(x = 1:3), original, 5, "x", function(data) {
      replicate <<- replicate + 1L
      data.frame(period = c("p1", "p2"), Index = c(100, if (replicate <= 3L) 120 else Inf))
    }, "synthetic"
  ))
  expect_length(captured$warnings, 1L)
  expect_match(captured$warnings, "p2 (3/5)", fixed = TRUE)
  expect_match(captured$warnings, "At least 4 required", fixed = TRUE)
  expect_equal(captured$result$bootstrap_variance[1], 0)
  expect_true(all(is.na(captured$result[2, c("bootstrap_variance", "lower_margin", "upper_margin")])))
  expect_identical(captured$result[names(original)], original)
  captured <- capture_bootstrap_warnings(calculate_bootstrap_margins(
    data.frame(x = 1:3), original, 2, "x", function(data) stop("fit failed"), "synthetic"
  ))
  expect_length(captured$warnings, 3L)
  expect_true(all(is.na(captured$result$bootstrap_variance)))
})

test_that("multiple methods receive reproducible bootstrap margins through the hub", {
  run <- function() bootstrap_test_run(c("median", "fisher"), margins = TRUE,
    bootstrap_replicates = 5, reference_period = "2020Q2", bootstrap_strata = "period")
  set.seed(74)
  first <- run()
  set.seed(74)
  second <- run()
  expect_identical(first, second)
  expect_named(first, c("median", "fisher"))
  original <- bootstrap_test_run(c("median", "fisher"), reference_period = "2020Q2")
  for (method in names(first)) {
    expect_identical(first[[method]][names(original[[method]])], original[[method]])
    expect_true(all(is.finite(first[[method]]$bootstrap_variance)))
    expect_equal(first[[method]]$Index[first[[method]]$period == "2020Q2"], 100)
  }
})

test_that("bootstrap calls the same hub with settings preserved and margins disabled", {
  real_hub <- calculate_hedonic_index
  seen <- list()
  local_mocked_bindings(calculate_hedonic_index = function(...) {
    args <- list(...)
    seen[[length(seen) + 1L]] <<- args
    data.frame(period = c("2020Q1", "2020Q2", "2021Q1"), Index = c(100, 120, 140))
  })
  result <- real_hub(
    dataset = bootstrap_test_data(), method = "laspeyres", period_variable = "period",
    dependent_variable = "price", numerical_variables = "area", categorical_variables = NULL,
    reference_period = "2020Q2", number_of_observations = FALSE, imputation = TRUE,
    parallel = TRUE, margins = TRUE, bootstrap_replicates = 2, bootstrap_strata = "period"
  )
  expect_length(seen, 2L)
  for (args in seen) {
    expect_identical(args$method, "laspeyres")
    expect_identical(args$margins, FALSE)
    expect_identical(args$index_contribution, FALSE)
    expect_identical(args$period_variable, "period")
    expect_identical(args$dependent_variable, "price")
    expect_identical(args$numerical_variables, "area")
    expect_null(args$categorical_variables)
    expect_identical(args$reference_period, "2020Q2")
    expect_identical(args$number_of_observations, FALSE)
    expect_identical(args$chained, FALSE)
    expect_identical(args$parallel, TRUE)
    expect_identical(args$imputation, TRUE)
    expect_equal(table(args$dataset$period), table(bootstrap_test_data()$period))
  }
  expect_true("Imputation" %in% names(result))
  expect_false("number_of_observations" %in% names(result))
})

test_that("bootstrap margins preserve the contribution output convention", {
  original <- bootstrap_test_run(index_contribution = TRUE)
  set.seed(75)
  result <- bootstrap_test_run(index_contribution = TRUE, margins = TRUE,
    bootstrap_replicates = 3, bootstrap_strata = "period")
  expect_named(result, c("Index", "Index_contribution"))
  expect_identical(result$Index_contribution, original$Index_contribution)
  expect_identical(result$Index[names(original$Index)], original$Index)
  expect_true(all(is.finite(result$Index$bootstrap_variance)))
})

test_that("generic bootstrap alignment distinguishes independent groups in the same period", {
  original <- data.frame(period = rep(c("p1", "p2"), each = 2),
                         type = rep(c("A", "B"), 2), Index = c(100, 200, 120, 240))
  replicate <- 0L
  result <- calculate_bootstrap_margins(
    data.frame(month = rep(c("p1", "p2"), each = 2), type = rep(c("A", "B"), 2)),
    original, 3, c("month", "type"), function(data) {
      replicate <<- replicate + 1L
      output <- original[4:1, ]
      output$Index <- c(20, 10, 4, 2) * replicate
      output
    }, "synthetic", index_variables = c("period", "type")
  )
  expect_identical(result[names(original)], original)
  expect_equal(result$bootstrap_variance, c(4, 16, 100, 400))
})

test_that("both hubs use the generic sampler with NULL or configured strata", {
  real_sampler <- sample_bootstrap_rows
  calls <- list()
  local_mocked_bindings(sample_bootstrap_rows = function(dataset, bootstrap_strata = NULL) {
    calls[[length(calls) + 1L]] <<- list(strata = bootstrap_strata)
    real_sampler(dataset, bootstrap_strata)
  })
  dataset <- data.frame(month = rep(c(202101, 202102), each = 6),
    price = c(100, 120, 150, 190, 220, 280, 110, 130, 160, 200, 230, 290),
    appraisal = 100, vintage = 2019, type = "A", region = rep(c("north", "south"), 6))
  for (strata in list(NULL, "month", c("month", "region"))) {
    for (hub in c("hedonic", "spar")) {
      calls <- list()
      set.seed(87)
      if (hub == "hedonic") {
        calculate_hedonic_index(dataset, "median", "month", "price",
          margins = TRUE, bootstrap_replicates = 2, bootstrap_strata = strata)
      } else {
        calculate_spar_index(dataset, "arithmetic", "month", "price", "appraisal",
          c("vintage", "type"), margins = TRUE, bootstrap_replicates = 2, bootstrap_strata = strata)
      }
      expect_length(calls, 2L)
      for (call in calls) expect_identical(call$strata, strata)
    }
  }
})

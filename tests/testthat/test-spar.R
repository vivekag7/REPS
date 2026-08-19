data("aritmethic_data", package = "REPS")

spar_test_args <- function(dataset = aritmethic_data) {
  list(
    dataset = dataset,
    period_variable = "Period",
    dependent_variable = "Price",
    appraisal_variable = "Appraisal Value",
    grouping_variables = c("Appraisal Year", "Property Type")
  )
}

get_spar_test_group <- function(dataset, period) {
  dataset[
    dataset[["Period"]] == period &
      dataset[["Appraisal Year"]] == 2019L &
      dataset[["Property Type"]] == "A",
  ]
}

calculate_expected_spar_ratio <- function(dataset, method) {
  prices <- dataset[["Price"]]
  appraisals <- dataset[["Appraisal Value"]]
  complete_pairs <- !is.na(prices) & !is.na(appraisals)
  prices <- prices[complete_pairs]
  appraisals <- appraisals[complete_pairs]

  switch(
    method,
    arithmetic = mean(prices) / mean(appraisals),
    geometric = exp(mean(log(prices))) / exp(mean(log(appraisals))),
    unweighted = mean(prices / appraisals)
  )
}

test_that("SPAR methods calculate their intended aggregates", {
  args <- spar_test_args()
  methods <- c("arithmetic", "geometric", "unweighted")
  results <- lapply(methods, function(method) {
    do.call(calculate_spar, c(args, list(method = method)))
  })
  names(results) <- methods

  reference_data <- get_spar_test_group(aritmethic_data, 202101L)
  comparison_data <- get_spar_test_group(aritmethic_data, 202102L)

  for (method in methods) {
    result <- results[[method]]
    reference_row <- result$period == "202101" &
      result[["Appraisal Year"]] == 2019L &
      result[["Property Type"]] == "A"
    comparison_row <- result$period == "202102" &
      result[["Appraisal Year"]] == 2019L &
      result[["Property Type"]] == "A"

    expected_reference <- calculate_expected_spar_ratio(reference_data, method)
    expected_comparison <- calculate_expected_spar_ratio(comparison_data, method)

    expect_equal(result$ratio_numerator[reference_row], expected_reference)
    expect_equal(
      result$growth_rate[comparison_row],
      expected_comparison / expected_reference
    )
    expect_equal(result$Index[comparison_row], 100 * result$growth_rate[comparison_row])
    expect_true(all(c("period", "Index") %in% names(result)))
  }
})

test_that("SPAR hub mirrors multi-method hedonic output", {
  args <- spar_test_args()
  methods <- c("arithmetic", "geometric", "unweighted")
  result <- do.call(calculate_spar, c(args, list(method = methods)))

  expect_type(result, "list")
  expect_named(result, methods)
  expect_true(all(vapply(result, is.data.frame, logical(1))))
  expect_true(all(vapply(result, nrow, integer(1)) == 120L))

  selected <- do.call(
    calculate_spar,
    c(args, list(method = c("unweighted", "arithmetic")))
  )
  expect_named(selected, c("unweighted", "arithmetic"))
})

test_that("SPAR chaining is performed independently by property type", {
  result <- do.call(
    calculate_spar,
    c(
      spar_test_args(),
      list(
        method = "arithmetic",
        index_type_variable = "Property Type",
        chained = TRUE
      )
    )
  )

  apartments <- result[result[["Property Type"]] == "A", ]
  expected_growth <- numeric(nrow(apartments))
  expected_growth[1] <- 1
  for (i in seq.int(2L, nrow(apartments))) {
    starts_segment <- as.integer(apartments$period[i]) %% 100L == 1L
    expected_growth[i] <- if (starts_segment) {
      apartments$growth_rate[i]
    } else {
      apartments$growth_rate[i] / apartments$growth_rate[i - 1L]
    }
  }

  expect_equal(apartments$growth_factor, expected_growth)
  expect_equal(apartments$Index, 100 * cumprod(expected_growth))
  expect_equal(apartments$Index[1], 100)
  expect_equal(
    apartments$chaining_true_false,
    as.integer(apartments$period) %% 100L == 1L &
      seq_len(nrow(apartments)) > 1L
  )
})

test_that("SPAR calculations use complete price-appraisal pairs", {
  dataset <- aritmethic_data
  target_rows <- which(
    dataset[["Period"]] == 202101L &
      dataset[["Appraisal Year"]] == 2019L &
      dataset[["Property Type"]] == "A"
  )
  dataset[["Price"]][target_rows[2L]] <- NA_real_

  result <- do.call(
    calculate_spar,
    c(spar_test_args(dataset), list(method = "arithmetic"))
  )
  reference_row <- result$period == "202101" &
    result[["Appraisal Year"]] == 2019L &
    result[["Property Type"]] == "A"
  complete_rows <- target_rows[c(1L, 3L)]

  expect_equal(
    result$numerator_numerator[reference_row],
    mean(dataset[["Price"]][complete_rows])
  )
  expect_equal(
    result$numerator_denominator[reference_row],
    mean(dataset[["Appraisal Value"]][complete_rows])
  )
})

test_that("SPAR validation rejects invalid inputs and ambiguous references", {
  args <- spar_test_args()

  expect_error(
    do.call(calculate_spar, c(args, list(method = "unknown"))),
    "Invalid method"
  )
  expect_error(
    do.call(calculate_spar, c(args, list(method = "all"))),
    "Invalid method"
  )
  expect_error(
    do.call(
      calculate_spar,
      c(
        args,
        list(
          method = c("arithmetic", "geometric", "unweighted"),
          chained = FALSE,
          parallel = NA
        )
      )
    ),
    "parallel"
  )
  expect_error(
    do.call(
      calculate_spar,
      c(
        args,
        list(
          method = "arithmetic",
          chained = TRUE,
          index_type_variable = "not_a_group"
        )
      )
    ),
    "index_type_variable"
  )
  expect_error(
    do.call(
      calculate_spar,
      c(
        args,
        list(
          method = "arithmetic",
          reference_period_pattern = "0[12]$"
        )
      )
    ),
    "at most one period"
  )

  nonpositive <- aritmethic_data
  nonpositive[["Appraisal Value"]][1L] <- 0
  expect_error(
    do.call(
      calculate_spar,
      c(spar_test_args(nonpositive), list(method = "arithmetic"))
    ),
    "strictly positive"
  )
})

test_that("SPAR groups without a reference period are reported and omitted", {
  extra_group <- aritmethic_data[
    aritmethic_data[["Appraisal Year"]] == 2019L &
      aritmethic_data[["Property Type"]] == "A" &
      aritmethic_data[["Period"]] == 202102L,
  ]
  extra_group[["Appraisal Year"]] <- 2018L
  dataset <- rbind(aritmethic_data, extra_group)

  result <- expect_warning(
    do.call(
      calculate_spar,
      c(spar_test_args(dataset), list(method = "arithmetic"))
    ),
    "1 group.*will be omitted"
  )

  expect_false(2018L %in% result[["Appraisal Year"]])
})

test_that("SPAR chaining rejects ambiguous type-period observations", {
  dataset <- aritmethic_data
  dataset$Region <- "north"
  duplicate_series <- dataset
  duplicate_series$Region <- "south"
  dataset <- rbind(dataset, duplicate_series)

  expect_error(
    calculate_spar(
      dataset = dataset,
      method = "arithmetic",
      period_variable = "Period",
      dependent_variable = "Price",
      appraisal_variable = "Appraisal Value",
      grouping_variables = c("Appraisal Year", "Property Type", "Region"),
      index_type_variable = "Property Type",
      chained = TRUE
    ),
    "one observation per period"
  )
})

test_that("packaged aritmethic_data has the documented structure", {
  expect_s3_class(aritmethic_data, "data.frame")
  expect_equal(nrow(aritmethic_data), 360L)
  expect_named(
    aritmethic_data,
    c("Period", "Appraisal Year", "Price", "Property Type", "Appraisal Value")
  )
  expect_equal(
    sort(unique(aritmethic_data[["Appraisal Year"]])),
    c(2019L, 2020L)
  )
  expect_false(anyNA(aritmethic_data))
  expect_true(all(aritmethic_data[["Price"]] > 0))
  expect_true(all(aritmethic_data[["Appraisal Value"]] > 0))
})

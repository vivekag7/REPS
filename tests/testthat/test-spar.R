spar_test_data <- function() {
  data.frame(
    month = rep(c(202101, 202102), each = 2), vintage = 2019,
    type = "A", price = c(100, 300, 200, 600), appraisal = c(100, 200, 100, 300)
  )
}

spar_test_run <- function(dataset = spar_test_data(), method = "arithmetic", ...) {
  args <- list(dataset = dataset, method = method, period_variable = "month",
               dependent_variable = "price", appraisal_variable = "appraisal",
               grouping_variables = c("vintage", "type"))
  do.call(calculate_spar, utils::modifyList(args, list(...), keep.null = TRUE))
}

test_that("SPAR aggregates and normalization match hand calculations", {
  expected <- list(arithmetic = c(4 / 3, 2), geometric = c(sqrt(1.5), 2),
                   unweighted = c(1.25, 2))
  for (method in names(expected)) {
    result <- spar_test_run(method = method)
    expect_s3_class(result, "data.frame")
    expect_equal(result$period, c("202101", "202102"))
    expect_equal(result$ratio_numerator, expected[[method]])
    expect_equal(result$ratio_denominator, rep(expected[[method]][1], 2))
    expect_equal(result$growth_rate, expected[[method]] / expected[[method]][1])
    expect_equal(result$Index, expected[[method]] / expected[[method]][1] * 100)
    expect_true(all(c("vintage", "type") %in% names(result)))
  }
  expect_equal(spar_test_run()$numerator_numerator, c(200, 400))
  expect_equal(spar_test_run()$numerator_denominator, c(150, 200))
  expect_equal(spar_test_run(method = "geometric")$numerator_numerator,
               c(sqrt(30000), sqrt(120000)))
})

test_that("all SPAR variants use complete price-appraisal pairs", {
  dataset <- spar_test_data()
  extras <- dataset[rep(c(1, 3), each = 2), ]
  extras$price <- rep(c(NA, 1e9), 2)
  extras$appraisal <- rep(c(1e9, NA), 2)
  dataset <- rbind(dataset, extras)
  dataset$unused <- NA_real_
  for (method in supported_spar_index_methods()) {
    expect_equal(spar_test_run(dataset, method), spar_test_run(method = method))
  }
  dataset$price[dataset$month == 202102] <- NA_real_
  expect_error(spar_test_run(dataset), "no complete price-appraisal pair")
  expect_error(spar_test_run(transform(spar_test_data(), price = NA_real_)), "no complete")
})

test_that("SPAR custom and quarterly reference patterns are respected", {
  result <- spar_test_run(reference_period_pattern = "02$")
  expect_equal(result$Index, c(200 / 3, 100))
  quarterly <- spar_test_data()
  quarterly$month <- rep(c("2021Q1", "2021Q2"), each = 2)
  expect_equal(spar_test_run(quarterly, reference_period_pattern = "Q1$")$Index, c(100, 150))
  expect_error(spar_test_run(reference_period_pattern = "0[12]$"), "at most one period")
})

test_that("missing references warn once and omit groups before multi-method runs", {
  dataset <- spar_test_data()
  extra <- dataset[dataset$month == 202102, ]
  extra$type <- "B"
  result <- NULL
  expect_warning(result <- spar_test_run(rbind(dataset, extra),
    method = c("arithmetic", "unweighted")), "1 group.*omitted")
  expect_equal(result$arithmetic, spar_test_run())
  expect_equal(result$unweighted, spar_test_run(method = "unweighted"))

  for (chained in c(FALSE, TRUE)) {
    empty <- NULL
    expect_warning(empty <- spar_test_run(extra, chained = chained,
      index_type_variable = "type"), "1 group.*omitted")
    expect_s3_class(empty, "data.frame")
    expect_equal(nrow(empty), 0L)
    expect_true(all(c("period", "Index", "vintage", "type") %in% names(empty)))
  }
})

test_that("SPAR method routing follows the named-list convention", {
  methods <- c("arithmetic", "geometric", "unweighted")
  result <- spar_test_run(method = methods)
  expect_named(result, methods)
  for (method in methods) expect_equal(result[[method]], spar_test_run(method = method))
  expect_named(spar_test_run(method = c("UNWEIGHTED", "Arithmetic", "unweighted")),
               c("unweighted", "arithmetic"))
  for (method in list(NULL, character(), NA_character_, 1, "all", "unknown", "")) {
    expect_error(spar_test_run(method = method), "method")
  }
  expect_error(get_spar_index_method_function("all"), "Invalid method")
  expect_false("type_variable" %in% names(formals(calculate_spar)))
})

spar_chain_test_data <- function() {
  data.frame(
    month = rep(c(202101, 202112, 202201, 202202), 2),
    vintage = rep(c(2019, 2019, 2020, 2020), 2), type = rep(c("A", "B"), each = 4),
    price = c(100, 120, 300, 330, 100, 150, 400, 320), appraisal = 100
  )
}

test_that("SPAR chains each independent series using hand-calculated levels", {
  dataset <- spar_chain_test_data()
  for (method in supported_spar_index_methods()) {
    result <- spar_test_run(dataset, method, chained = TRUE, index_type_variable = "type")
    a <- result[result$type == "A", ]
    b <- result[result$type == "B", ]
    # The next reference period carries the previous endpoint (no boundary jump).
    expect_equal(a$Index, c(100, 120, 120, 132))
    expect_equal(b$Index, c(100, 150, 150, 120))
    expect_equal(a$growth_factor, c(1, 1.2, 1, 1.1))
    expect_equal(a$chaining_true_false, c(FALSE, FALSE, TRUE, FALSE))
    expect_equal(spar_test_run(dataset, method, chained = TRUE,
      index_type_variable = "type", base_value = 200)$Index, 2 * result$Index)
    expect_equal(spar_test_run(dataset[nrow(dataset):1, ], method,
      chained = TRUE, index_type_variable = "type"), result)
  }
  dataset$region <- "north"
  expect_equal(spar_test_run(dataset, chained = TRUE,
    grouping_variables = c("vintage", "type", "region"),
    index_type_variable = c("type", "region"))$Index, c(100, 120, 120, 132, 100, 150, 150, 120))
})

test_that("SPAR chaining accepts numeric-like periods and custom modulo", {
  dataset <- spar_chain_test_data()
  dataset$month <- as.character(dataset$month)
  expect_equal(spar_test_run(dataset, chained = TRUE, index_type_variable = "type")$Index,
               c(100, 120, 120, 132, 100, 150, 150, 120))
  dataset$month <- rep(c(20211, 20214, 20221, 20222), 2)
  expect_equal(spar_test_run(dataset, chained = TRUE, index_type_variable = "type",
    month_modulo = 10, reference_period_pattern = "1$")$Index,
    c(100, 120, 120, 132, 100, 150, 150, 120))
  dataset$month <- paste0(dataset$month, "Q")
  expect_error(spar_test_run(dataset, chained = TRUE, index_type_variable = "type",
    reference_period_pattern = "1Q$"), "numeric")
  expect_error(spar_test_run(chained = TRUE, index_type_variable = "type",
    reference_period_pattern = "02$"), "reference period.*segment start")
})

test_that("ambiguous chain series and misaligned segments are rejected", {
  dataset <- spar_chain_test_data()
  dataset$region <- "north"
  other <- dataset
  other$region <- "south"
  dataset <- rbind(dataset, other)
  expect_error(spar_test_run(dataset, chained = TRUE,
    grouping_variables = c("vintage", "type", "region"),
    index_type_variable = "type"), "one observation per period")
  expect_equal(nrow(spar_test_run(dataset, chained = TRUE,
    grouping_variables = c("vintage", "type", "region"),
    index_type_variable = c("type", "region"))), 16L)
  dataset <- spar_test_data()
  dataset$month[3:4] <- 202202
  expect_error(spar_test_run(dataset, chained = TRUE, index_type_variable = "type"), "one numeric segment")
  dataset <- spar_test_data()
  dataset$month <- as.character(dataset$month)
  duplicate <- dataset[3:4, ]
  duplicate$month <- "0202102"
  expect_error(spar_test_run(rbind(dataset, duplicate), chained = TRUE,
    index_type_variable = "type"), "one observation per period")
})

test_that("SPAR validates data, identifiers, prices, and regular expressions", {
  for (bad in list(list(), matrix(1, 2, 2), spar_test_data()[FALSE, ])) {
    expect_error(spar_test_run(bad), "data frame")
  }
  for (column in c("month", "price", "appraisal", "vintage", "type")) {
    dataset <- spar_test_data()
    dataset[[column]] <- NULL
    expect_error(spar_test_run(dataset), "missing")
  }
  for (column in c("price", "appraisal")) {
    expect_error(spar_test_run(spar_test_data(), dependent_variable = "type"), "distinct")
    for (bad in c(0, -1, Inf, -Inf)) {
      dataset <- spar_test_data()
      dataset[[column]][1] <- bad
      expect_error(spar_test_run(dataset), "positive and finite")
    }
    dataset <- spar_test_data()
    dataset[[column]] <- as.character(dataset[[column]])
    expect_error(spar_test_run(dataset), "numeric")
  }
  for (column in c("month", "type", "vintage")) {
    dataset <- spar_test_data()
    dataset[[column]][1] <- NA
    expect_error(spar_test_run(dataset), "identifiers")
    dataset[[column]] <- ""
    expect_error(spar_test_run(dataset), "identifiers")
  }
  for (bad in list(NULL, NA_character_, "", c("01$", "02$"), "[")) {
    expect_error(spar_test_run(reference_period_pattern = bad), "regular expression")
  }
  expect_error(spar_test_run(period_variable = c("month", "vintage")), "column name")
  expect_error(spar_test_run(grouping_variables = c("type", "type")), "unique column")
  expect_error(spar_test_run(grouping_variables = NULL), "column names")
  dataset <- spar_test_data()
  dataset$Index <- "A"
  expect_error(spar_test_run(dataset, grouping_variables = "Index"), "reserved")
})

test_that("SPAR validates every supplied option including inactive chain options", {
  for (bad in list(NA, 1, NULL, c(TRUE, FALSE))) {
    expect_error(spar_test_run(parallel = bad), "parallel")
    expect_error(spar_test_run(chained = bad), "chained")
  }
  for (bad in list(NA_real_, 0, -1, Inf, 1i, c(100, 200), "100", NULL)) {
    expect_error(spar_test_run(base_value = bad), "base_value")
  }
  for (bad in list(NA_real_, 0, 1, 1.5, Inf, 1i, c(10, 100), "100", NULL)) {
    expect_error(spar_test_run(month_modulo = bad), "month_modulo")
  }
  expect_error(spar_test_run(chained = TRUE), "index_type_variable")
  expect_error(spar_test_run(index_type_variable = "missing"), "index_type_variable")
  expect_error(spar_test_run(chained = TRUE, index_type_variable = "price"), "index_type_variable")
})

test_that("sequential and parallel multi-method SPAR results agree", {
  for (chained in c(FALSE, TRUE)) {
    args <- list(dataset = spar_chain_test_data(), method = supported_spar_index_methods(),
                 chained = chained, index_type_variable = "type")
    sequential <- do.call(spar_test_run, args)
    parallel <- do.call(spar_test_run, c(args, list(parallel = TRUE)))
    expect_equal(parallel, sequential, tolerance = 1e-12)
  }
})

test_that("SPAR results plot as one series and reject grouped output", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  single <- spar_test_run()
  multiple <- spar_test_run(method = supported_spar_index_methods())
  expect_silent(plot_price_index(single))
  expect_silent(plot_price_index(multiple))
  expect_silent(plot_price_index(list(arithmetic = single, shorter = single[1, ])))
  grouped <- spar_test_run(spar_chain_test_data(), chained = TRUE, index_type_variable = "type")
  expect_error(plot_price_index(grouped), "filter grouped output to one series")
  expect_error(plot_price_index(list(grouped = grouped)), "filter grouped output to one series")
  expect_error(plot_price_index(single[FALSE, ]), "finite numeric")
  expect_error(plot_price_index(list()), "valid multi-method output")
})

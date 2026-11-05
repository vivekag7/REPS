# Stored expectations come directly from the supplied script on the exact
# arithmetic_data selection, not from calculate_spar_index(). The full CSV and the
# script stay external. Set both environment variables below to run the full
# original-script regression during development and package checks.

spar_reference_columns <- function(data) {
  original <- c("Periode", "WozPeilJaar", "Woningtype", "ratio_numerator", "ratio_jan",
                "growth_rate", "growth_rate_long", "yoy_change", "chain_long", "index_long")
  data <- as.data.frame(data[original])
  names(data) <- c("period", "appraisal_year", "property_type", "ratio_numerator", "ratio_denominator",
                   "growth_rate", "growth_rate_long", "yoy_change", "chain_long", "Index")
  data$period <- as.character(data$period)
  data
}

expect_spar_reference_values <- function(expected, actual, tolerance = 1e-10) {
  keys <- c("period", "appraisal_year", "property_type")
  expect_false(anyDuplicated(expected[keys]) > 0L)
  expect_false(anyDuplicated(actual[keys]) > 0L)
  joined <- merge(expected, actual[names(expected)], by = keys, all = TRUE,
                  suffixes = c("_reference", "_actual"))
  expect_equal(nrow(joined), nrow(expected))
  expect_equal(nrow(joined), nrow(actual))
  for (column in setdiff(names(expected), keys)) {
    difference <- abs(joined[[paste0(column, "_actual")]] - joined[[paste0(column, "_reference")]])
    expect_false(anyNA(difference), info = column)
    expect_true(all(difference <= tolerance), info = paste(column, "maximum absolute difference:", max(difference)))
  }
}

test_that("arithmetic_data matches the independently generated script expectations", {
  data("arithmetic_data", package = "REPS")
  fixture <- readRDS(test_path("test_data", "spar_reference_output.rds"))
  result <- calculate_spar_index(
    arithmetic_data, c("arithmetic", "geometric", "unweighted"),
    period_variable = "period", dependent_variable = "price",
    appraisal_variable = "appraisal_value", grouping_variables = c("appraisal_year", "property_type"),
    index_type_variable = "property_type", chained = TRUE
  )
  for (method in names(fixture$expected)) {
    expect_spar_reference_values(fixture$expected[[method]], result[[method]])
  }
})

test_that("full external SPAR source and original script match the public API", {
  source_path <- Sys.getenv("REPS_SPAR_SOURCE")
  reference_path <- Sys.getenv("REPS_SPAR_REFERENCE_SCRIPT")
  skip_if(!nzchar(source_path) || !nzchar(reference_path) ||
            !file.exists(source_path) || !file.exists(reference_path),
          "External test2.csv and reference script not configured")
  fixture <- readRDS(test_path("test_data", "spar_reference_output.rds"))
  expect_identical(unname(tools::md5sum(source_path)), fixture$provenance$source_md5)
  expect_identical(unname(tools::md5sum(reference_path)), fixture$provenance$reference_md5)

  # Evaluate only the four inspected function definitions. Never source library,
  # network, View(), or development statements from the external script.
  wanted <- c("calculate_spar", paste0("calculate_spar_", names(fixture$expected)))
  definitions <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
      is.symbol(expression[[2L]]) && as.character(expression[[2L]]) %in% wanted &&
      is.call(expression[[3L]]) && identical(expression[[3L]][[1L]], as.name("function"))
  }, as.list(parse(reference_path)))
  expect_length(definitions, 4L)
  reference <- new.env(parent = asNamespace("dplyr"))
  for (definition in definitions) eval(definition, envir = reference)
  source_data <- utils::read.csv2(source_path, stringsAsFactors = FALSE)
  expect_equal(nrow(source_data), fixture$provenance$source_rows)
  reference_args <- list(
    dataset = source_data, period_variable = "Periode", dependent_variable = "KoopSom",
    appraisal_variable = "WozWaarde", grouping_variables = c("WozPeilJaar", "Woningtype"),
    type_variable = "Woningtype", reference_period_pattern = "01$"
  )
  reference_hub <- do.call(reference$calculate_spar, c(reference_args, list(method = "all")))
  result <- calculate_spar_index(
    source_data, names(fixture$expected), period_variable = "Periode", dependent_variable = "KoopSom",
    appraisal_variable = "WozWaarde", grouping_variables = c("WozPeilJaar", "Woningtype"),
    index_type_variable = "Woningtype", chained = TRUE
  )
  for (method in names(fixture$expected)) {
    original <- do.call(reference[[paste0("calculate_spar_", method)]], reference_args)
    expect_equal(original$index_long, reference_hub[[paste0("index_long_", method)]], tolerance = 1e-10)
    actual <- result[[method]]
    names(actual)[match(c("WozPeilJaar", "Woningtype"), names(actual))] <- c("appraisal_year", "property_type")
    expect_spar_reference_values(spar_reference_columns(original), actual)
  }
})

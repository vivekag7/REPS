test_that("calculate_contribution_indexmutation returns row-level output by default", {
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

  result <- calculate_contribution_indexmutation(
    dataset = dataset,
    index_output = original_index,
    period_variable = "period",
    calculate_index_function = function(target_dataset) {
      data.frame(
        period = c("2020Q1", "2020Q2"),
        Index = c(100, 115 + nrow(target_dataset[target_dataset$period == "2020Q2", ]))
      )
    },
    index_mutation_period = "2020Q2"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("price", "area", "Index_original", "PoP_original") %in% names(result)))
  expect_false(".indexmutation_unit_id" %in% names(result))
})

test_that("calculate_contribution_indexmutation validates regular index output", {
  expect_error(
    calculate_contribution_indexmutation(
      dataset = data.frame(period = "2020Q1", price = 100),
      index_output = list(Index = data.frame(period = "2020Q1", Index = 100)),
      period_variable = "period",
      calculate_index_function = function(target_dataset) target_dataset
    ),
    "regular index data.frame"
  )
})

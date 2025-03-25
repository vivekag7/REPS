test_that("calculate_geometric_average works correctly", {
  a <- c(1, 2, 3, 4)
  expect_equal(round(calculate_geometric_average(a), 4), round(2.213364, 4))
  
  # Check that NA values are ignored
  b <- c(1, 2, NA, 4)
  expect_equal(round(calculate_geometric_average(b), 4), round(exp(mean(log(c(1, 2, 4)))), 4))
  
  # Non-numeric input should trigger an error
  expect_error(calculate_geometric_average(c("a", "b", "c")), "Values is not \\(fully\\) numeric")
})

test_that("calculate_index returns expected index series", {
  periods <- c("2020Q1", "2020Q2", "2020Q3")
  values <- c(100, 110, 90)
  
  result <- calculate_index(periods, values, reference_period = "2020Q1")
  expect_equal(round(result[1], 1), 100)
  expect_equal(round(result[2], 1), 110)
  expect_equal(round(result[3], 1), 90)
  
  # Check fallback to first period if reference not specified
  result2 <- calculate_index(periods, values)
  expect_equal(result, result2)
  
  # Non-numeric values trigger an error
  expect_error(calculate_index(periods, c("a", "b", "c")), "values variable is not \\(fully\\) numeric")
  
  # Mismatch in vector lengths triggers an error
  expect_error(calculate_index(c("2020Q1", "2020Q2"), c(100, 110, 120)), "not of the same length")
})

test_that("show_progress_loop runs silently", {
  expect_output(show_progress_loop(1, 5), "Progress:") 
  expect_message(show_progress_loop(5, 5), "Done!")    
})
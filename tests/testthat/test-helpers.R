test_that("Geometric average works", {
  a <- c(1,2,3,4)
  expect_equal(round(calculate_geometric_average(a),4),round(2.213364,4))
})

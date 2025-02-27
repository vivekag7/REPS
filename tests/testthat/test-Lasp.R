


test_that("Period variable must be alphanumeric", {
    expect_equal(
    unique(stringr::str_detect(unlist(Calculate_laspeyres(dataset = data_constraxion,
                                   , period_variable = c("period")
                                   , dependent_variable = c('price')
                                   , continious_variables = c('floor_area')
                                   , categorical_variables = c('neighbourhood_code')
                                   , log_dependent = TRUE
                                   , reference_period = 2015
                                   , number_of_observations = TRUE
                                   , imputation = TRUE
                                   , bootstrap = 10)[1]),"[0-9]")),  TRUE)

  expect_equal(ncol(Calculate_laspeyres(dataset = data_constraxion,
                                                                         , period_variable = c("period")
                                                                         , dependent_variable = c('price')
                                                                         , continious_variables = c('floor_area')
                                                                         , categorical_variables = c('neighbourhood_code')
                                                                         , log_dependent = TRUE
                                                                         , reference_period = 2015
                                                                         , number_of_observations = TRUE
                                                                         , imputation = TRUE
                                                                         , bootstrap = 10)),7)
})

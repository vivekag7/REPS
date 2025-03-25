test_that("Output Laspeyres is valid", {
  tbl_laspeyres <- calculate_laspeyres(dataset = data_constraxion
                                       , period_variable = c("period")
                                       , dependent_variable = c('price')
                                       , continious_variables = c('floor_area')
                                       , categorical_variables = c('neighbourhood_code')
                                       , log_dependent = TRUE
                                       , reference_period = 2015
                                       , number_of_observations = TRUE
                                       , imputation = TRUE)
})

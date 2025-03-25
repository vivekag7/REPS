test_that("Output Paasche is valid", {
  tbl_paasche <- calculate_paasche(dataset = data_constraxion
                                   , period_variable = c("period")
                                   , dependent_variable = c('price')
                                   , continious_variables = c('floor_area')
                                   , categorical_variables = c('neighbourhood_code')
                                   , log_dependent = TRUE
                                   , reference_period = 2015
                                   , number_of_observations = TRUE
                                   , imputation = TRUE)
})

# initialize parameters to avoid "no visible binding for global variable" when running check()

utils::globalVariables(c("dataset", "dependent_variable","continuous_variables", 
                         "independent_variables", 
                         "number_of_observations", "period_list", 
                         "period_var_temp","period", "value", "bind_rows"))

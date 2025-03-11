# initialize parameters for calculate_hedonic_imputation to avoid "no visible binding for global variable" when running check()

utils::globalVariables(c("dataset", "dependent_variable", 
                         "independent_variables", "log_dependent", 
                         "number_of_observations", "period_list", 
                         "period_var_temp","period", "value"))

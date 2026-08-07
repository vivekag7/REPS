library(dplyr)

#' Calculate SPAR Index (Hub Function)
#'
#' @author Kai Cheung
#' @param dataset A data.frame with input data
#' @param period_variable Name of the period variable (string)
#' @param dependent_variable Name of the dependent variable (mostly sales price)
#' @param appraisal_variable Name of the appraisal variable, the variable of the denominator
#' @param grouping_variables Name of the variable(s) to group for the index calculation e.g. appraisal year and building_type
#' @param index_type_variable Name of the column/variables on which type must be separated to make a fair calculation (e.g. column with rows like apartment, corner house etc.)
#' @param reference_period_pattern string of the pattern that must be taken to calculate at start of the pattern. e.g. if your dataset starts with '01' and you want the base period at january, then "01$"
#' @param method The calculation method to use: "arithmetic", "geometric", or "unweighted".
#' @param chaining set the chaining on TRUE if you want an chained SPAR index and FALSE if you just want the short term index
#' @param base_value fixed on 100, cause of the index theory starts with 100 
#' @param month_modulo fixed on 100, checks whether the period must be chained or not
#'
#' @return A dataframe with the calculated growth_rate and an added 'method_used' column.

calculate_spar <- function(dataset,
                           period_variable,
                           dependent_variable,
                           appraisal_variable,
                           grouping_variables,
                           index_type_variable,
                           reference_period_pattern = "01$",
                           method = c("arithmetic", "geometric", "unweighted", "all"), 
                           chaining = FALSE,           
                           base_value = 100,           
                           month_modulo = 100) {       
  
  # 1. Custom validation (Allows multiple methods!)
  valid_methods <- c("arithmetic", "geometric", "unweighted", "all")
  if (!all(method %in% valid_methods)) stop("Invalid method chosen.")
  if ("all" %in% method) method <- c("arithmetic", "geometric", "unweighted")
  
  results_list <- list()
  
  # 2. Loop through each requested method
  for (m in method) {
    
    # Calculate the base SPAR index
    res <- switch(m,
                  "arithmetic" = calculate_spar_arithmetic(dataset, period_variable, dependent_variable, appraisal_variable, grouping_variables, reference_period_pattern),
                  "geometric"  = calculate_spar_geometric(dataset, period_variable, dependent_variable, appraisal_variable, grouping_variables, reference_period_pattern),
                  "unweighted" = calculate_spar_unweighted(dataset, period_variable, dependent_variable, appraisal_variable, grouping_variables, reference_period_pattern)
    )
    
    # REQUIREMENT 1: Rename the index column so it includes the method name
    names(res)[names(res) == "growth_rate"] <- paste0("growth_rate_", m)
    
    # Conditionally run the chaining function
    if (chaining) {
      res <- make_chain(
        df = res,
        periode_col = period_variable,
        woning_col = index_type_variable, 
        growth_col = paste0("growth_rate_", m), 
        base_value = base_value,
        month_modulo = month_modulo
      )
      
      # REQUIREMENT 2: Rename the chained index columns to include the method name
      names(res)[names(res) == "chain_index"] <- paste0("chain_index_", m)
      names(res)[names(res) == "growth"] <- paste0("growth_factor_", m)
      
      # Only keep the TRUE/FALSE chaining marker for the first method to avoid duplicates
      if (m != method[1]) res$chaining_true_false <- NULL
    }
    
    # Tag the method used
    res$method_used <- m
    results_list[[m]] <- res
  }
  
  # 3. Combine all results into one wide dataframe
  join_cols <- c(period_variable, grouping_variables)
  final_df <- results_list[[1]]
  
  if (length(results_list) > 1) {
    for (i in 2:length(results_list)) {
      df_to_join <- results_list[[i]] %>% select(-any_of("method_used"))
      final_df <- left_join(final_df, df_to_join, by = join_cols)
    }
    final_df$method_used <- NULL 
  }
  
  return(final_df)
}





#' calculate_spar_arithmetic
#'
#' @author Kai Cheung
#' @param dataset  A data.frame with input data
#' @param period_variable Name of the period variable (string)
#' @param dependent_variable Name of the dependent variable (mostly sales price)
#' @param appraisal_variable Name of the appraisal variable, the variable of the denominator
#' @param grouping_variables Name of the variable(s) to group for the index calculation e.g. appraisal year and building_type
#' @param reference_period_pattern string of the pattern that must be taken to calculate at start of the pattern. e.g. if your dataset starts with '01' and you want the base period at january, then "01$"
#'
#' @returns a dataframe with a short term arithmetic SPAR indices (for each year)
#' @export
#'
#' @examples
calculate_spar_arithmetic <- function(dataset
                                      , period_variable
                                      , dependent_variable
                                      , appraisal_variable
                                      , grouping_variables
                                      , reference_period_pattern = "01$") {
  
  
  group_variables <- c(period_variable, grouping_variables)
  
  df_numerator <- dataset %>%
    group_by(across(all_of(group_variables))) %>%
    summarise(
      numerator_numerator = mean(.data[[dependent_variable]], na.rm = TRUE),
      numerator_denominator = mean(.data[[appraisal_variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(ratio_numerator = numerator_numerator /  numerator_denominator)
  
  df_denominator <- df_numerator %>%
    filter(grepl(reference_period_pattern, .data[[period_variable]])) %>%
    select(
      all_of(grouping_variables),
      ratio_denominator = ratio_numerator
    )
  
  df_numerator %>%
    left_join(df_denominator, by = grouping_variables) %>%
    mutate(growth_rate = ratio_numerator / ratio_denominator)
  
}


#' calculate_spar_geometric
#'
#' @author Kai Cheung
#' @param dataset  A data.frame with input data
#' @param period_variable Name of the period variable (string)
#' @param dependent_variable Name of the dependent variable (mostly sales price)
#' @param appraisal_variable Name of the appraisal variable, the variable of the denominator
#' @param grouping_variables Name of the variable(s) to group for the index calculation e.g. appraisal year and building_type
#' @param reference_period_pattern string of the pattern that must be taken to calculate at start of the pattern. e.g. if your dataset starts with '01' and you want the base period at january, then "01$"
#'
#' @returns a dataframe with a short term geometric SPAR indices (for each year)
#' @export
#'
#' @examples
calculate_spar_geometric <- function(dataset
                                     , period_variable
                                     , dependent_variable
                                     , appraisal_variable
                                     , grouping_variables
                                     , reference_period_pattern = "01$") {
  
  
  group_variables <- c(period_variable, grouping_variables)
  
  df_numerator <- dataset %>%
    group_by(across(all_of(group_variables))) %>%
    summarise(
      numerator_numerator = exp(mean(log(.data[[dependent_variable]]), na.rm = TRUE)),
      numerator_denominator = exp(mean(log(.data[[appraisal_variable]]), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(ratio_numerator = numerator_numerator / numerator_denominator)
  
  df_denominator <- df_numerator %>%
    filter(grepl(reference_period_pattern, .data[[period_variable]])) %>%
    select(
      all_of(grouping_variables),
      ratio_denominator = ratio_numerator
    )
  
  df_numerator %>%
    left_join(df_denominator, by = grouping_variables) %>%
    mutate(growth_rate = ratio_numerator / ratio_denominator)
}


#' calculate_spar_unweighted
#'
#' @author Kai Cheung
#' @param dataset  A data.frame with input data
#' @param period_variable Name of the period variable (string)
#' @param dependent_variable Name of the dependent variable (mostly sales price)
#' @param appraisal_variable Name of the appraisal variable, the variable of the denominator
#' @param grouping_variables Name of the variable(s) to group for the index calculation e.g. appraisal year and building_type
#' @param reference_period_pattern string of the pattern that must be taken to calculate at start of the pattern. e.g. if your dataset starts with '01' and you want the base period at january, then "01$"
#'
#' @returns a dataframe with a short term unweighted SPAR indices (for each year)
#' @export
#'
#' @examples
calculate_spar_unweighted <- function(dataset
                                      , period_variable
                                      , dependent_variable
                                      , appraisal_variable
                                      , grouping_variables
                                      , reference_period_pattern = "01$") {
  
  
  group_variables <- c(period_variable, grouping_variables)
  
  df_numerator <- dataset %>%
    group_by(across(all_of(group_variables))) %>%
    summarise(
      ratio_numerator = mean(
        .data[[dependent_variable]] / .data[[appraisal_variable]],
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  df_denominator <- df_numerator %>%
    filter(grepl(reference_period_pattern, .data[[period_variable]])) %>%
    select(
      all_of(grouping_variables),
      ratio_denominator = ratio_numerator
    )
  
  df_numerator %>%
    left_join(df_denominator, by = grouping_variables) %>%
    mutate(growth_rate = ratio_numerator / ratio_denominator)
}


#' chaining SPAR
#'
#' @author Kai Cheung
#' @param df A data.frame with input data
#' @param periode_col this will be generated automatically, cause its an internal function, used for period
#' @param woning_col this will be generated automatically, cause its an internal function, used for building type
#' @param growth_col this will be generated automatically, cause its an internal function, used for growth_rate
#' @param base_value base value is standard on 100, cause of the fundamental theory of price indices
#'
#' @returns a chained SPAR indice
#' @export
#'
#' @examples
make_chain <- function(
    df,
    periode_col = period_variable,
    woning_col = index_type_variable,      
    growth_col = "growth_rate",
    base_value = 100,
    month_modulo = 100   
) {
  # --- basic validation ---
  if (!is.data.frame(df)) stop("`df` must be a data.frame / tibble.")
  if (!all(c(periode_col, woning_col, growth_col) %in% names(df))) {
    stop("One or more specified columns are missing from the data.")
  }
  
  # --- processing ---
  df <- df %>%
    # 1. clean / prepare
    mutate(
      !!sym(periode_col) := as.integer(as.character(!!sym(periode_col))),
      !!sym(woning_col) := trimws(as.character(!!sym(woning_col)))
    ) %>%
    arrange(!!sym(woning_col), !!sym(periode_col)) %>%
    
    # 2. group and calculate chain
    group_by(across(all_of(woning_col))) %>%
    mutate(
      # mark new series start
      chaining_true_false = !!sym(periode_col) %% month_modulo == 1 & row_number() > 1,
      
      # growth factor vs previous period
      growth = case_when(
        row_number() == 1 ~ 1,
        chaining_true_false ~ !!sym(growth_col) / 1,  
        TRUE              ~ !!sym(growth_col) / lag(!!sym(growth_col))
      ),
      
      # chained index
      chain_index = base_value * cumprod(growth)
    ) %>%
    ungroup()
  
  return(df)
}



# reading testdata
test <- read.csv2("//cbsp.nl/besturend/EBN/EBD/PG/Werk/KCNG/package_R/REPS_git/test1.csv")

# Test 1: arithmatic SPAR
spar_arithmetic <- calculate_spar_arithmetic(
  dataset = test,
  period_variable = "Periode",
  dependent_variable = "KoopSom",
  appraisal_variable = "WozWaarde",
  grouping_variables = c("WozPeilJaar", "Woningtype"),
  reference_period_pattern = "01$")

View(spar_arithmetic)


# Test 2: geometric SPAR
spar_geometric <- calculate_spar_geometric(
  dataset = test,
  period_variable = "Periode",
  dependent_variable = "KoopSom",
  appraisal_variable = "WozWaarde",
  grouping_variables = c("WozPeilJaar", "Woningtype"),
  reference_period_pattern = "01$")

View(spar_geometric)


# Test 3: unweighted SPAR
spar_unweighted <- calculate_spar_unweighted(
  dataset = test,
  period_variable = "Periode",
  dependent_variable = "KoopSom",
  appraisal_variable = "WozWaarde",
  grouping_variables = c("WozPeilJaar", "Woningtype"),
  reference_period_pattern = "01$")

View(spar_unweighted)


# Test 4: using vignette/HUB function SPAR
df_spar_chain <- calculate_spar(
  dataset = test,
  period_variable = "Periode",
  dependent_variable = "KoopSom",
  appraisal_variable = "WozWaarde",
  grouping_variables = c("WozPeilJaar", "Woningtype"),
  index_type_variable = "Woningtype",
  reference_period_pattern = "01$",
  method = "all",
  chaining = TRUE 
  #base_value = 100,
  #month_modulo = 100
)





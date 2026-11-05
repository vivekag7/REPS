# Rebuild the example from the external SPAR development source, from repo root:
# Sys.setenv(REPS_SPAR_SOURCE = "/path/to/test2.csv")
# source("data-raw/arithmetic_data.R")
# Alternatively: Rscript data-raw/arithmetic_data.R /path/to/test2.csv
# The full CSV remains external and is not distributed with the package.

source_path <- Sys.getenv("REPS_SPAR_SOURCE")
if (!nzchar(source_path)) {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments)) source_path <- arguments[1L]
}
if (!nzchar(source_path) || !file.exists(source_path)) {
  stop("Set REPS_SPAR_SOURCE or supply the external test2.csv path.")
}
stopifnot(file.exists("DESCRIPTION"), read.dcf("DESCRIPTION")[1, "Package"] == "REPS")

source_data <- utils::read.csv2(source_path, stringsAsFactors = FALSE)
source_columns <- c("Periode", "WozPeilJaar", "KoopSom", "Woningtype", "WozWaarde")
stopifnot(all(source_columns %in% names(source_data)))
source_data <- source_data[source_columns]
names(source_data) <- c("period", "appraisal_year", "price", "property_type", "appraisal_value")
source_data$period <- as.integer(source_data$period)
source_data$appraisal_year <- as.integer(source_data$appraisal_year)
source_data$price <- as.numeric(source_data$price)
source_data$property_type <- as.character(source_data$property_type)
property_type_labels <- c(
  "Appartement" = "Apartment",
  "Hoekwoning" = "End-of-terrace house",
  "Tussenwoning" = "Mid-terrace house",
  "Twee onder 1 kap" = "Semi-detached house"
)
stopifnot(all(source_data$property_type %in% names(property_type_labels)))
source_data$property_type <- unname(property_type_labels[source_data$property_type])
source_data$appraisal_value <- as.numeric(source_data$appraisal_value)
stopifnot(
  !anyNA(source_data), all(is.finite(source_data$price)),
  all(is.finite(source_data$appraisal_value)), all(source_data$price > 0),
  all(source_data$appraisal_value > 0), all(source_data$period %% 100 %in% 1:12)
)

# Preserve every period/vintage/type stratum. Take up to three price-ranked,
# approximately quartile-spaced observations; break ties by appraisal value.
# Selection depends only on the five data columns, not source row order or RNG.
strata <- c("period", "appraisal_year", "property_type")
grouped <- dplyr::group_by(source_data, dplyr::across(dplyr::all_of(strata)))
selected <- dplyr::group_modify(grouped, function(.x, .y) {
  .x <- .x[order(.x$price, .x$appraisal_value), , drop = FALSE]
  n <- nrow(.x)
  keep <- min(3L, n)
  ranks <- as.integer(round(seq_len(keep) / (keep + 1L) * (n + 1L)))
  .x[unique(pmax(1L, pmin(n, ranks))), , drop = FALSE]
})
arithmetic_data <- as.data.frame(dplyr::ungroup(selected))[names(source_data)]
arithmetic_data <- arithmetic_data[order(
  arithmetic_data$period, arithmetic_data$property_type, arithmetic_data$appraisal_year,
  arithmetic_data$price, arithmetic_data$appraisal_value
), ]
row.names(arithmetic_data) <- NULL

sort_keys <- function(data) {
  keys <- unique(data[strata])
  keys <- keys[do.call(order, keys), ]
  row.names(keys) <- NULL
  keys
}
stopifnot(
  nrow(arithmetic_data) <= 1500L, !anyNA(arithmetic_data),
  identical(sort_keys(arithmetic_data), sort_keys(source_data)),
  length(unique(arithmetic_data$period %/% 100)) > 2L,
  length(unique(arithmetic_data$appraisal_year)) > 2L,
  length(unique(arithmetic_data$property_type)) > 1L,
  all(c(1L, 12L) %in% (arithmetic_data$period %% 100)),
  length(unique(arithmetic_data$price)) > 10L,
  length(unique(arithmetic_data$appraisal_value)) > 10L
)
usethis::use_data(arithmetic_data, overwrite = TRUE, compress = "xz", version = 2)

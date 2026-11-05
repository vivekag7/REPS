arithmetic_data <- read.csv(
  "data-raw/arithmetic_data.csv",
  stringsAsFactors = FALSE,
  colClasses = c("integer", "integer", "numeric", "character", "numeric")
)

usethis::use_data(arithmetic_data, overwrite = TRUE, compress = "xz", version = 2)

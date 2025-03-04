## code to prepare `data_constraxion` dataset goes here



#' A real estate example data frame
#'
#' A subset of data from a fictitious real estate data frame containing transaction prices and some
#'  categorical and numerical characteristics of each dwelling

#' @format
#' A data frame with 7,800 rows and 6 columns:
#' \describe{
#'   \item{period}{A (string) vetcor indicating a time period }
#'   \item{price}{A (string) vector indicating the transaction price of the dwelling  }
#'   \item{floor_area}{A real valued vector of (the logarithm) of the floor area
#'   of the dwelling}
#'   \item{dist_trainstation}{A real valued vector of (the logarithm) of the
#'   distance of the dwelling to the nearest train station}
#'   \item{neighbourhood_code}{A categorical code/string reffering to the neighborhood
#'   the dwelling belongs to}
#'   \item{dummy_large_city}{A vector indicating whether the dwelling belongs to a
#'   large city or not}

#' }
#' @source  A fictitious dataset for illustration purposes

#'examples data_constraxion
#' data(data_constraxion)      # Lazy data

"data_constraxion"
data_constraxion <- read.csv2(here::here("data-raw", "EXAMPLE_data.csv"))

names(data_constraxion) = c("period", "price", "floor_area", "dist_trainstation", "neighbourhood_code", "dummy_large_city")

usethis::use_data(data_constraxion, overwrite = TRUE)

usethis::use_data_raw("data_constraxion")

#' @title Calculate Price Index (Defunct)
#' @description This function has been removed and replaced. Please use \code{\link{calculate_hedonic_index}} instead.
#' @param ... Arguments are ignored as the function is defunct.
#' @export
calculate_price_index <- function(...) {
  
  # Throws a hard error and immediately stops the user's script
  .Defunct(
    new = "calculate_hedonic_index", 
    package = "REPS", 
    msg = "Error: 'calculate_price_index()' has been permanently removed from this package. Please use 'calculate_hedonic_index()' from now on."
  )
  
}
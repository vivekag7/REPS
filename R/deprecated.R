#' @title Calculate Price Index (Deprecated)
#' @description This function has been renamed. Please use \code{\link{calculate_hedonic_index}} instead.
#' @param ... Arguments passed to \code{calculate_hedonic_index}.
#' @export
calculate_price_index <- function(...) {
  
  # Throws a soft warning but allows the script to continue
  .Deprecated(
    new = "calculate_hedonic_index", 
    package = "REPS", 
    msg = "Warning: 'calculate_price_index()' is deprecated and will be removed in a future version. Please use 'calculate_hedonic_index()' from now on."
  )
  
  # Passes all arguments to the new function so the user's code still runs perfectly
  calculate_hedonic_index(...)
}
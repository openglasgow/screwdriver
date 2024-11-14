
#' Is Date
#'
#' Determine if an object is a date
#'
#' @param x object to test, probably a vector.
#'
#' @return TRUE or FALSE
#' @export

is_date <- function (x) {

  class(x) == "Date"

}

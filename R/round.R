
#' Round Number
#'
#' Rounds a numeric vector mathematically, i.e. .5 is rounded up, to a specified
#' number of decimal places. This is in contrast to the base round function
#' which follows the IEC 60559 standard and rounds to the even digit in these
#' cases and sometimes catches users off-guard.
#'
#' @param x A numeric vector of values to round.
#' @param digits Integer indicating the number of decimal places to round to.
#'   The default is 0, indicating rounding to the nearest whole number.
#'
#' @return A numeric vector with the rounded values.
#' @export
#' @examples
#' round_number(1.55, 1)  # Returns 1.6
#' round_number(c(-1.55, 2.55), 1)  # Returns c(-1.6, 2.6)

round_number <- function(x, digits = 0) {

  # posneg <- sign(x)
  # z <- abs(x) * 10 ^ digits
  # z <- z + 0.5 + sqrt(.Machine$double.eps)
  # z <- trunc(z)
  # z <- z / 10 ^ digits
  # z * posneg

  posneg <- sign(x)
  z <- abs(x) * 10 ^ digits
  z <- z + 0.5  # Assuming custom rounding logic needed here
  z <- trunc(z)
  z <- z / 10 ^ digits
  z * posneg

}

#' Round to Nearest Multiple
#'
#' Rounds a numeric vector to the nearest specified multiple, similar to the
#' MROUND function in Excel. This function uses mathematical rounding - see
#' \code{\link{round_number}} for more information on this.
#'
#' @param x A numeric vector of values to round.
#' @param k The multiple to which the values should be rounded. Must be
#'   a positive number.
#'
#' @return A numeric vector with the values rounded to the nearest multiple of \code{k}.
#' @export
#' @examples
#' round_nearest(123, 5)  # Returns 125
#' round_nearest(c(2, 7), 5)  # Returns c(0, 5)

round_nearest <- function (x, k) {

  round_number(x / k) * k

}

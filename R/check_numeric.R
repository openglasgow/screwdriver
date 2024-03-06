
#' Check Numeric Vector Validity
#'
#' Performs validations on a numeric vector, including type (integer or double),
#' presence of NA, Inf, -Inf, NaN, range of values, and whether the elements are whole numbers.
#'
#' @param x A numeric vector to be validated.
#' @param type Optional; specify "integer" or "double" to check for a specific numeric type.
#' @param allow_NA A logical value indicating whether `NA` values are allowed in `x`.
#' @param allow_inf A logical value indicating whether `Inf` and `-Inf` values are allowed in `x`.
#' @param allow_nan A logical value indicating whether `NaN` values are allowed in `x`.
#' @param value_range A vector of length 2 indicating the minimum and maximum values allowed.
#'   Use `NA` to omit the minimum or maximum constraint.
#' @param check_whole A logical value indicating whether to check that all elements are whole numbers.
#' @param x_name Optional character string specifying the name of the vector being checked.
#'   Used in error messages for clarity.
#'
#' @return `TRUE` if `x` passes all specified checks, otherwise
#'   an error is thrown detailing the discrepancy.
#'
#' @examples
#' check_numeric(c(1, 2, 3), type = "integer", allow_NA = FALSE, allow_inf = FALSE,
#'               allow_nan = FALSE, value_range = c(1, 5), check_whole = TRUE, x_name = "my_vector")
#'
#' @export

check_numeric <- function(x, type = NULL, allow_NA = TRUE, allow_inf = TRUE, allow_nan = TRUE,
                          value_range = c(NA, NA), check_whole = FALSE, x_name = "Numeric vector") {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be a numeric vector.", x_name))
  }

  if (!is.null(type) && !(type %in% c("integer", "double"))) {
    stop("Type must be either 'integer' or 'double'.")
  }

  # Exclude NA values from the integer type check or explicitly handle them
  if (type == "integer" && !all((x[!is.na(x)] %% 1) == 0)) {
    stop(sprintf("%s contains non-integer values but was expected to be integer.", x_name))
  }

  if (!allow_NA && any(is.na(x))) {
    stop(sprintf("%s contains NA values but NAs are not allowed.", x_name))
  }

  if (!allow_inf && (any(is.infinite(x), na.rm = TRUE))) {
    stop(sprintf("%s contains infinite values but Inf/-Inf are not allowed.", x_name))
  }

  if (!allow_nan && any(is.nan(x))) {
    stop(sprintf("%s contains NaN values but NaNs are not allowed.", x_name))
  }

  if (!is.na(value_range[1]) && any(x < value_range[1], na.rm = TRUE)) {
    stop(sprintf("%s contains values less than the minimum allowed %d.", x_name, value_range[1]))
  }

  if (!is.na(value_range[2]) && any(x > value_range[2], na.rm = TRUE)) {
    stop(sprintf("%s exceeds the maximum allowed %d.", x_name, value_range[2]))
  }

  if (check_whole && !all((x[!is.na(x)] == floor(x[!is.na(x)])))) {
    stop(sprintf("%s contains non-whole numbers but was expected to contain only whole numbers.", x_name))
  }

  return(TRUE)
}


# check_numeric <- function(x, type = NULL, allow_NA = TRUE, allow_inf = TRUE, allow_nan = TRUE,
#                           value_range = c(NA, NA), check_whole = FALSE, x_name = "Numeric vector") {
#
#   print(x)
#   print(value_range)
#
#
#   if (!is.numeric(x)) {
#     stop(sprintf("%s must be a numeric vector.", x_name))
#   }
#
#   if (!is.null(type) && !type %in% c("integer", "double")) {
#     stop("Type must be either 'integer' or 'double'.")
#   }
#
#   if (!is.null(type)) {
#     if (type == "integer" && !all(x == as.integer(x))) {
#       stop(sprintf("%s contains non-integer values but was expected to be integer.", x_name))
#     }
#     if (type == "double" && !all(x == as.numeric(x))) {
#       stop(sprintf("%s contains non-double values but was expected to be double.", x_name))
#     }
#   }
#
#   if (!allow_NA && any(is.na(x))) {
#     stop(sprintf("%s contains NA values but NAs are not allowed.", x_name))
#   }
#
#   if (!allow_inf && (any(is.infinite(x)))) {
#     stop(sprintf("%s contains infinite values but Inf/-Inf are not allowed.", x_name))
#   }
#
#   if (!allow_nan && any(is.nan(x))) {
#     stop(sprintf("%s contains NaN values but NaNs are not allowed.", x_name))
#   }
#
#   if (!all(is.na(value_range)) && length(value_range) == 2) {
#     if (!is.na(value_range[1]) && any(x < value_range[1])) {
#       stop(sprintf("%s contains values less than the minimum allowed %d.", x_name, value_range[1]))
#     }
#     if (!is.na(value_range[2]) && any(x > value_range[2])) {
#       stop(sprintf("%s exceeds the maximum allowed %d.", x_name, value_range[2]))
#     }
#   }
#
#   if (check_whole && !all(x == floor(x))) {
#     stop(sprintf("%s contains non-whole numbers but was expected to contain only whole numbers.", x_name))
#   }
#
#   return(TRUE)
# }

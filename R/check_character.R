#' Check Character Vector Validity
#'
#' Performs validations on an atomic character vector, including length, allowed values,
#' character count within elements, and allowance of NAs.
#'
#' @param x An atomic character vector to be validated.
#' @param length_range A single positive whole number or a vector of length 2 indicating
#'   the exact, minimum, or maximum length of `x`. Use `NA` to omit the minimum
#'   or maximum constraint.
#' @param allowed_values A character vector specifying the allowed values for elements in `x`.
#'   If `NULL`, this check is skipped.
#' @param num_chars A single positive whole number or a vector of length 2 indicating
#'   the exact, minimum, or maximum number of characters for each element in `x`.
#'   Use `NA` to omit the minimum or maximum constraint.
#' @param allow_NA A logical value indicating whether `NA` values are allowed in `x`.
#' @param x_name Optional character string specifying the name of the vector being checked.
#'   Used in error messages for clarity.
#'
#' @return `TRUE` if `x` passes all specified checks, otherwise
#'   an error is thrown detailing the first encountered discrepancy.
#'
#' @examples
#' check_character(c("apple", "banana"), length_range = 2, allowed_values = c("apple", "banana", "cherry"),
#'                 num_chars = c(5, NA), allow_NA = FALSE, x_name = "fruit vector")
#'
#' @export

check_character <- function(x, length_range = NULL, allowed_values = NULL,
                            num_chars = NULL, allow_NA = TRUE,
                            x_name = "Character vector") {

  if (!is.character(x)) {

    stop(sprintf("%s must be a character vector.", x_name))

  }

  if (!is.null(length_range)) {

    len <- length(x)

    if (length(length_range) == 1 && len != length_range) {

      stop(sprintf("%s does not have the exact length of %d.", x_name, length_range))

    } else if (length(length_range) == 2) {

      if (!is.na(length_range[1]) && len < length_range[1]) {
        stop(sprintf("%s has fewer elements than the minimum required %d.", x_name, length_range[1]))
      }

      if (!is.na(length_range[2]) && len > length_range[2]) {
        stop(sprintf("%s exceeds the maximum allowed %d elements.", x_name, length_range[2]))
      }

    }
  }

  if (!allow_NA && any(is.na(x))) {

    stop(sprintf("%s contains NA values but NAs are not allowed.", x_name))

  }

  if (!is.null(allowed_values) && !all(x %in% allowed_values)) {

    invalid_values <- x[!x %in% allowed_values]
    stop(sprintf("%s contains values not in the allowed set: %s", x_name,
                 paste(unique(invalid_values), collapse = ", ")))

  }

  if (!is.null(num_chars)) {

    char_counts <- nchar(x)

    if (length(num_chars) == 1 && !all(char_counts == num_chars)) {

      stop(sprintf("All elements in %s must have exactly %d characters.",
                   x_name, num_chars))

    } else if (length(num_chars) == 2) {

      if (!is.na(num_chars[1]) && any(char_counts < num_chars[1])) {

        stop(sprintf("Some elements in %s have fewer characters than the minimum %d.",
                     x_name, num_chars[1]))

      }

      if (!is.na(num_chars[2]) && any(char_counts > num_chars[2])) {

        stop(sprintf("Some elements in %s exceed the maximum %d characters.",
                     x_name, num_chars[2]))

      }
    }
  }

  return(TRUE)
}

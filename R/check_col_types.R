
#' Check Column Types in a Data Frame
#'
#' Validates if a data frame's columns match the specified types or classes.
#' A named vector should be supplied for specific column checks, or an unnamed vector
#' for position-based checks. Throws an error for missing columns or type mismatches.
#'
#' @param df A data frame to be checked.
#' @param col_types A vector of types/classes expected for the data frame's columns.
#'   If named, matches column names to types. If unnamed, assumes order by position.
#' @param df_name Optional character string specifying the name of the data frame
#'   being checked, used in error messages.
#'
#' @return `TRUE` if the data frame columns match the specified types, otherwise
#'   an error is thrown.
#'
#' @examples
#' df_example <- data.frame(name = as.character(1:10), age = as.integer(21:30))
#' check_col_types(df_example, c(name = "character", age = "integer"), df_name = "df_example")
#' check_col_types(df_example, c("character", "integer"), df_name = "df_example")
#'
#' @export

check_col_types <- function (df, col_types, df_name = "df") {

  if (!is.data.frame(df)) {
    stop(sprintf("%s must be a data frame.", df_name))
  }

  if (is.null(names(col_types))) {

    if (length(col_types) != ncol(df)) {

      stop(sprintf("The number of types provided does not match the number of columns in %s.", df_name))

    }

    col_names <- names(df)

  } else {

    col_names <- names(col_types)

    if (!all(col_names %in% names(df))) {

      missing_cols <- col_names[!col_names %in% names(df)]
      stop(sprintf("%s is missing the specified columns: %s", df_name, paste(missing_cols, collapse = ", ")))

    }
  }

  actual_types <- sapply(df[col_names], function(column) class(column)[1])
  expected_types <- if (is.null(names(col_types))) col_types else col_types[col_names]

  mismatches <- which(actual_types != expected_types)

  if (length(mismatches) > 0) {

    error_message <- paste(sapply(mismatches, function(i) {
      sprintf("Column '%s' expected type '%s' but got '%s'", col_names[i], expected_types[i], actual_types[i])
    }), collapse = "; ")

    stop(error_message)
  }

  return(TRUE)
}

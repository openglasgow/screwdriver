
#' Comprehensive Data Frame Checks
#'
#' Performs a series of checks on a data frame, including validations for the number of rows,
#' the number of columns, presence and order of specified column names, and column types.
#'
#' @param df A data frame to be validated.
#' @param num_rows A single positive whole number or a vector of length 2 indicating
#'   the exact, minimum, or maximum number of rows allowed. Use `NA` to omit the minimum
#'   or maximum constraint. If `NULL`, this check is skipped.
#' @param num_cols A single positive whole number or a vector of length 2 indicating
#'   the exact, minimum, or maximum number of columns allowed. Use `NA` to omit the minimum
#'   or maximum constraint. If `NULL`, this check is skipped.
#' @param col_names A character vector specifying the column names expected to be present
#'   in the data frame in the given order. If `NULL`, this check is skipped.
#' @param col_types A named vector of types/classes expected for the data frame's columns.
#'   If `NULL`, this check is skipped.
#' @param df_name Optional character string specifying the name of the data frame being checked.
#'   Used in error messages for clarity.
#'
#' @return `TRUE` if the data frame passes all specified checks, otherwise
#'   an error is thrown detailing the first encountered discrepancy.
#'
#' @examples
#' df_example <- data.frame(name = as.character(1:10), age = as.integer(21:30))
#' check_df(df_example, num_rows = c(1, NA), num_cols = 2,
#'          col_names = c("name", "age"), col_types = c(name = "character", age = "integer"),
#'          df_name = "df_example")
#'
#' @export

check_df <- function (df, num_rows = NULL, num_cols = NULL, col_names = NULL,
                      col_types = NULL, df_name = "df") {

  if (!is.null(num_rows)) {

    check_num_rows(df, num_rows, df_name)

  }

  if (!is.null(num_cols)) {

    check_num_cols(df, num_cols, df_name)

  }

  if (!is.null(col_names)) {

    check_col_names(df, col_names, df_name)

  }

  if (!is.null(col_types)) {

    if (is.null(names(col_types)) && !is.null(col_names)) {

      col_types <- setNames(col_types, col_names)

    }

    check_col_types(df, col_types, df_name)

  }

  return(TRUE)

}


#' Check Presence of Specified Column Names in a Data Frame
#'
#' Validates if a data frame contains all of the specified column names.
#' An optional `df_name` argument enhances error messages with the data frame's name.
#'
#' @param df A data frame to check for specified column names.
#' @param col_names A character vector specifying the column names expected to be present in the data frame.
#' @param df_name Optional character string specifying the name of the data frame
#'   being checked. This name is used in error messages for clarity.
#'
#' @return `TRUE` if the data frame contains all of the specified column names, otherwise
#'   an error is thrown detailing the missing columns.
#'
#' @examples
#' df_example <- data.frame(x = 1:10, y = letters[1:10])
#' check_col_names(df_example, c("x", "y"), df_name = "df_example") # Columns are present
#' check_col_names(df_example, c("x", "z"), df_name = "df_example") # Column 'z' is missing
#'
#' @export

check_col_names <- function(df, col_names, df_name = "df") {

  if (!is.data.frame(df)) {

    stop(sprintf("%s must be a data frame", df_name))

  }

  missing_cols <- setdiff(col_names, names(df))

  if (length(missing_cols) > 0) {

    stop(sprintf("%s is missing the following columns: %s",
                 df_name, paste(missing_cols, collapse = ", ")))

  }

  return(TRUE)
}

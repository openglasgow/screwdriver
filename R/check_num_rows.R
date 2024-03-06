
#' Check the Number of Rows in a Data Frame
#'
#' Validates the number of rows in a given data frame against specified criteria.
#'
#' @param df A data frame whose rows are to be checked.
#' @param num_rows A single positive whole number or a vector of length 2
#'   indicating the exact, minimum, or maximum number of rows allowed. Use `NA`
#'   to omit the minimum or maximum constraint.
#' @param df_name Optional character string specifying the name of the data frame
#'   being checked. This name is used in error messages for clarity.
#'
#' @return `TRUE` if the data frame meets the row count criteria, otherwise
#'   an error is thrown detailing the discrepancy.
#'
#' @examples
#' df_example <- data.frame(x = 1:10)
#' check_num_rows(df_example, 10, df_name = "df_example") # Exact number of rows
#' check_num_rows(df_example, c(5, 15), df_name = "df_example") # Range of rows
#' check_num_rows(df_example, c(NA, 20), df_name = "df_example") # Maximum number of rows
#' check_num_rows(df_example, c(5, NA), df_name = "df_example") # Minimum number of rows
#'
#' @export

check_num_rows <- function(df, num_rows, df_name = "df") {

  if (!is.data.frame(df)) {
    stop(sprintf("%s must be a data frame", df_name))
  }

  num_rows_valid <- is.numeric(num_rows) && length(num_rows) <= 2 && all(num_rows >= 0 | is.na(num_rows))

  if (!num_rows_valid) {
    stop(sprintf("The `num_rows` argument in %s must be a single positive number or a vector of length 2 with non-negative values.", df_name))
  }

  num_rows_check <- switch(
    length(num_rows),
    `1` = nrow(df) != num_rows,
    `2` = (!is.na(num_rows[1]) && nrow(df) < num_rows[1]) || (!is.na(num_rows[2]) && nrow(df) > num_rows[2])
  )

  if (num_rows_check) {

    if (length(num_rows) == 1) {

      stop(sprintf("%s does not have the exact number of %d rows.", df_name, num_rows))

    } else {

      message <- sprintf("%s row count is outside the specified range [%s, %s].",
                         df_name,
                         ifelse(is.na(num_rows[1]), "NA", num_rows[1]),
                         ifelse(is.na(num_rows[2]), "NA", num_rows[2]))
      stop(message)

    }
  }

  return(TRUE)

}


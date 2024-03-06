
#' Check the Number of Columns in a Data Frame
#'
#' Validates the number of columns in a given data frame against specified criteria.
#'
#' @param df A data frame whose columns are to be checked.
#' @param num_cols A single positive whole number or a vector of length 2
#'   indicating the exact, minimum, or maximum number of columns allowed. Use `NA`
#'   to omit the minimum or maximum constraint.
#' @param df_name Optional character string specifying the name of the data frame
#'   being checked. This name is used in error messages for clarity.
#'
#' @return `TRUE` if the data frame meets the column count criteria, otherwise
#'   an error is thrown detailing the discrepancy.
#'
#' @examples
#' df_example <- data.frame(x = 1:10, y = letters[1:10])
#' check_num_cols(df_example, 2, df_name = "df_example") # Exact number of columns
#' check_num_cols(df_example, c(1, 3), df_name = "df_example") # Range of columns
#' check_num_cols(df_example, c(NA, 4), df_name = "df_example") # Maximum number of columns
#' check_num_cols(df_example, c(2, NA), df_name = "df_example") # Minimum number of columns
#'
#' @export

check_num_cols <- function(df, num_cols, df_name = "df") {

  if (!is.data.frame(df)) {

    stop(sprintf("%s must be a data frame", df_name))

  }

  if (!is.numeric(num_cols) || length(num_cols) > 2) {

    stop(sprintf("%s: `num_cols` must be a single number or a vector of length 2", df_name))

  }

  col_count <- ncol(df)

  if (length(num_cols) == 1 && col_count != num_cols) {

    stop(sprintf("%s does not have the exact number of %d columns.", df_name, num_cols))

  } else if (length(num_cols) == 2) {

    if (!is.na(num_cols[1]) && col_count < num_cols[1]) {

      stop(sprintf("%s has fewer than the minimum required %d columns.", df_name, num_cols[1]))

    }

    if (!is.na(num_cols[2]) && col_count > num_cols[2]) {

      stop(sprintf("%s exceeds the maximum allowed %d columns.", df_name, num_cols[2]))

    }
  }

  return(TRUE)
}

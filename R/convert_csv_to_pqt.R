
#' Convert CSV File to Parquet Format
#'
#' This function reads in a CSV file and converts it to Parquet format.
#'
#' @param file_in A character string specifying the path to the input CSV file.
#' @param file_out A character string specifying the path to the output Parquet file.
#'   Defaults to `NA`, which will use the same path and name as `file_in`,
#'   replacing the `.csv` extension with `.pqt`.
#'
#' @return This function returns `invisible(NA)`. The output file is saved as a Parquet file.
#' @importFrom vroom vroom
#' @importFrom arrow write_parquet
#'
#' @examples
#' \dontrun{
#' # Convert a CSV file to Parquet format with default output file name
#' convert_csv_to_pqt("data/input_data.csv")
#'
#' # Convert a CSV file to Parquet format with a specified output file name
#' convert_csv_to_pqt("data/input_data.csv", "data/output_data.pqt")
#' }
#'
#' @export

convert_csv_to_pqt <- function (file_in, file_out = NA) {

  df <- vroom(file_in, show_col_types = FALSE, progress = FALSE)

  if (is.na(file_out)) file_out <- sub("\\.csv$", ".pqt", file_in)

  write_parquet(df, file_out)

  return(invisible(NA))
}


#' Generate a Timestamp with Milliseconds
#'
#' The `get_time_stamp` function generates a timestamp string in the format
#' "YYYYMMDD-HHMMSSmmm", where `mmm` represents milliseconds. It captures the
#' current system time and formats it as a string suitable for file names or
#' logging purposes, ensuring precision down to milliseconds.
#'
#' @return A string representing the current timestamp in the format
#' "YYYYMMDD-HHMMSSmmm", where `mmm` is the milliseconds.
#'
#' @examples
#' # Generate a timestamp for the current time
#' get_time_stamp()
#'
#' @export

get_time_stamp <- function() {

  current_time <- Sys.time()

  timestamp <- withr::with_options(
    list(digits.secs = 3),
    format(current_time, "%Y%m%d-%H%M%OS3")
  )

  # Remove the dot before the milliseconds
  timestamp <- gsub("\\.", "-", timestamp)

  return(timestamp)
}



#' Calculate Age
#'
#' Calculate age in years based on a birth date and to date
#'
#' @param birth_date vector of dates
#' @param to_date vector of dates, or date of length 1, default = Sys.Date()
#' @param freq one of year, months or weeks. These are functions to pass from
#' the lubridate package.
#' @param set_negative_na if TRUE will change negative ages to NA
#' @param warn if TRUE will warn where dates are implausible
#'
#' @return age in years
#' @export

calc_age <- function (birth_date, to_date = Sys.Date(), freq = years,
                      set_negative_na = FALSE, warn = FALSE) {

  if (!is_date(birth_date)) stop ("birth_date must be a date")
  if (!is_date(to_date)) stop ("to_date must be a date")

  if (length(to_date) != length(birth_date) & length(to_date) != 1) {

    stop ("to_date must be length 1 or same length as birth date")

  }

  posixlt_to_date <- as.POSIXlt(to_date)
  posixlt_birth_date <- as.POSIXlt(birth_date)

  ages <- posixlt_to_date$year - posixlt_birth_date$year -
          ((posixlt_to_date$mon < posixlt_birth_date$mon) |
           ((posixlt_to_date$mon == posixlt_birth_date$mon) &
            (posixlt_to_date$mday < posixlt_birth_date$mday)))

  bad_birth_dates <- birth_date[birth_date > to_date]

  if (warn == TRUE) {

    if (length(bad_birth_dates) > 0) {

      warning ("one or more elements of to_date are on or after birth_date")

    }

  }

  if (set_negative_na == TRUE) {

    ages[birth_date > to_date] <- NA

  }

  return (ages)


}

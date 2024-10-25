
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

  if (!is.Date(birth_date)) stop ("birth_date must be a date")
  if (!is.Date(to_date)) stop ("to_date must be a date")

  if (length(to_date) != length(birth_date) & length(to_date) != 1) {

    stop ("to_date must be length 1 or same length as birth date")

  }

  ages <- trunc((birth_date %--% to_date) / freq(1))

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

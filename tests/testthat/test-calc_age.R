
test_that("calc_age works", {

  # Ages work

  expect_equal(calc_age(birth_date = as.Date("2022-07-01"),
                        to_date = as.Date("2023-03-01")),
               0)

  expect_equal(calc_age(birth_date = as.Date("2013-03-01"),
                        to_date = as.Date("2023-03-01")),
               10)

  expect_equal(calc_age(birth_date = as.Date("2013-03-02"),
                        to_date = as.Date("2023-03-01")),
               9)

  # An unusual case that would be wrong if we took an approximation approach
  # and was wrong in an old version of the function

  expect_equal(calc_age(as.Date("2000-03-01"), as.Date("2021-02-28")), 20)

  # Multiple ages work

  birth_dates <- as.Date(c("2022-07-01", "2013-03-01", "2013-03-02"))
  to_dates <- as.Date(c("2023-03-01", "2022-03-01", "2021-01-01"))

  expect_equal(calc_age(birth_dates, to_dates), c(0, 9, 7))
  expect_equal(calc_age(birth_dates, as.Date("2023-03-01")), c(0, 10, 9))

  # Bad input types fail

  expect_error(calc_age(birth_date = data.frame(as.Date("2013-03-02")),
                        to_date = as.Date("2023-03-01")))

  expect_error(calc_age(birth_date = as.Date("2013-03-02"),
                        to_date = data.frame(as.Date("2023-03-01"))))

  # if to_date not length 1 or length = length birth date then gives errors

  expect_error(calc_age(birth_dates, to_dates[-3]))

  # to_date < birth_date gives warnings and returns NA

  birth_dates[1] <- as.Date("2024-01-01")
  expect_warning(calc_age(birth_dates, to_dates, warn = TRUE))

  expect_equal(
    calc_age(birth_dates, to_dates, set_negative_na = TRUE), c(NA, 9, 7)
  )



})


test_that("check_numeric handles type checks correctly", {
  expect_true(check_numeric(1:3, type = "integer", x_name = "test_int"))
  expect_error(check_numeric(c(1, 2.5), type = "integer", x_name = "test_mix"))
})

test_that("check_numeric handles NA, Inf, and NaN checks correctly", {
  expect_error(check_numeric(c(1, NA), allow_NA = FALSE, x_name = "test_NA"))
  expect_error(check_numeric(c(1, Inf), allow_inf = FALSE, x_name = "test_Inf"))
  expect_error(check_numeric(c(1, NaN), allow_nan = FALSE, x_name = "test_NaN"))
})

test_that("check_numeric validates value range correctly", {
  expect_true(check_numeric(c(1, 5), value_range = c(1, 5), x_name = "test_range"))
  expect_error(check_numeric(c(0, 5), value_range = c(1, 5), x_name = "test_outside_range"))
})

test_that("check_numeric checks for whole numbers correctly", {
  expect_true(check_numeric(1:3, check_whole = TRUE, x_name = "test_whole"))
  expect_error(check_numeric(c(1, 2.5), check_whole = TRUE, x_name = "test_decimal"))
})


# Tests for round_number

test_that("round_number correctly rounds numbers", {
  expect_equal(round_number(1.55, 1), 1.6)
  expect_equal(round_number(-1.55, 1), -1.6)
  expect_equal(round_number(1.54, 1), 1.5)
  expect_equal(round_number(-1.54, 1), -1.5)
  expect_equal(round_number(c(-1.55, 2.55), 1), c(-1.6, 2.6))
})

test_that("round_number handles NAs, NaNs, Inf, and -Inf correctly", {
  expect_true(all(is.na(round_number(c(NA, NaN), 1))))
  expect_equal(round_number(Inf, 1), Inf)
  expect_equal(round_number(-Inf, 1), -Inf)
})

# Tests for round_nearest

test_that("round_nearest correctly rounds to nearest multiple", {
  expect_equal(round_nearest(123, 5), 125)
  expect_equal(round_nearest(127, 5), 125)
  expect_equal(round_nearest(-123, 5), -125)
  expect_equal(round_nearest(-127, 5), -125)
  expect_equal(round_nearest(c(2, 7), 5), c(0, 5))
  expect_equal(round_nearest(c(2, 7), 2.5), c(2.5, 7.5))
  expect_equal(round_nearest(c(2, 7.5), 5), c(0, 10))

})

test_that("round_nearest handles NAs, NaNs, Inf, and -Inf correctly", {
  expect_true(all(is.na(round_nearest(c(NA, NaN), 5))))
  expect_equal(round_nearest(Inf, 5), Inf)
  expect_equal(round_nearest(-Inf, 5), -Inf)
})


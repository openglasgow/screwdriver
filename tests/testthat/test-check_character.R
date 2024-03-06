
test_that("check_character handles length correctly", {
  expect_true(check_character(c("a", "b"), length_range = 2, x_name = "test_vector"))
  expect_error(check_character(c("a"), length_range = 2, x_name = "test_vector"))
})

test_that("check_character validates allowed values", {
  expect_true(check_character(c("a", "b"), allowed_values = c("a", "b", "c"), x_name = "test_vector"))
  expect_error(check_character(c("a", "d"), allowed_values = c("a", "b", "c"), x_name = "test_vector"))
})

test_that("check_character checks character counts within elements", {
  expect_true(check_character(c("ab", "cd"), num_chars = c(2, NA), x_name = "test_vector"))
  expect_error(check_character(c("ab", "cde"), num_chars = c(2, 2), x_name = "test_vector"))
})

test_that("check_character respects NA allowance", {
  expect_true(check_character(c("a", NA), allow_NA = TRUE, x_name = "test_vector"))
  expect_error(check_character(c("a", NA), allow_NA = FALSE, x_name = "test_vector"))
})

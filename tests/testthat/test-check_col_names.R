
df_example <- data.frame(x = 1:10, y = letters[1:10])

test_that("Function identifies present columns correctly", {
  expect_true(check_col_names(df_example, c("x", "y"), df_name = "df_example"))
})

test_that("Function detects missing columns correctly", {
  expect_error(check_col_names(df_example, c("x", "z"), df_name = "df_example"))
})

test_that("Function errors on non-data.frame input", {
  expect_error(check_col_names(1:10, c("x", "y"), df_name = "vector"))
})

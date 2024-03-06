
df_example <- data.frame(x = 1:10, y = letters[1:10])

test_that("Function correctly identifies the exact number of columns", {
  expect_true(check_num_cols(df_example, 2, df_name = "df_example"))
  expect_error(check_num_cols(df_example, 3, df_name = "df_example"))
})

test_that("Function handles minimum and maximum columns correctly", {
  expect_true(check_num_cols(df_example, c(1, 3), df_name = "df_example"))
  expect_error(check_num_cols(df_example, c(3, NA), df_name = "df_example"))
  expect_error(check_num_cols(df_example, c(NA, 1), df_name = "df_example"))
})

test_that("Function errors on non-data.frame input", {
  expect_error(check_num_cols(1:10, 1, df_name = "vector"))
})

test_that("Function validates `num_cols` argument structure", {
  expect_error(check_num_cols(df_example, "two", df_name = "df_example"))
  expect_error(check_num_cols(df_example, c(1, 2, 3), df_name = "df_example"))
})

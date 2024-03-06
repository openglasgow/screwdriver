
df_example <- data.frame(name = as.character(1:10), age = as.integer(21:30))

test_that("Function identifies correct types", {
  expect_true(check_col_types(df_example, c(name = "character", age = "integer"), df_name = "df_example"))
})

test_that("Function detects type mismatches", {
  expect_error(check_col_types(df_example, c(name = "factor", age = "numeric"), df_name = "df_example"))
})

test_that("Function handles unnamed types correctly", {
  expect_true(check_col_types(df_example, c("character", "integer"), df_name = "df_example"))
  expect_error(check_col_types(df_example, c("character"), df_name = "df_example"))
})

test_that("Function throws an error for missing columns", {
  expect_error(check_col_types(df_example, c(name = "character", height = "numeric"), df_name = "df_example"))
})


test_that("check_num_rows handles exact row count correctly", {
  df <- data.frame(x = 1:10)
  expect_true(check_num_rows(df, 10))
  expect_error(check_num_rows(df, 11))
})

test_that("check_num_rows handles range of rows correctly", {
  df <- data.frame(x = 1:10)
  expect_true(check_num_rows(df, c(5, 15)))
  expect_error(check_num_rows(df, c(11, 20)))
  expect_error(check_num_rows(df, c(1, 9)))
})

test_that("check_num_rows handles minimum rows correctly", {
  df <- data.frame(x = 1:10)
  expect_true(check_num_rows(df, c(1, NA)))
  expect_error(check_num_rows(df, c(11, NA)))
})

test_that("check_num_rows handles maximum rows correctly", {
  df <- data.frame(x = 1:10)
  expect_true(check_num_rows(df, c(NA, 10)))
  expect_error(check_num_rows(df, c(NA, 9)))
})

test_that("check_num_rows checks for data frame correctly", {
  not_df <- 1:10
  expect_error(check_num_rows(not_df, 10))
})

test_that("check_num_rows uses df_name in error messages", {

  df <- data.frame(x = 1:5)

  expect_error(check_num_rows(df, 10, df_name = "test_df"),
               "test_df does not have the exact number of 10 rows.")

  expect_error(check_num_rows(df, c(6, NA), df_name = "test_df"),
               regexp = "test_df row count is outside the specified range \\[6, NA\\]")

})

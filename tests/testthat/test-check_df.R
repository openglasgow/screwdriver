
# Sample data frame for testing

df_example <- data.frame(
  name = as.character(1:10),
  age = as.integer(21:30),
  stringsAsFactors = FALSE
)

# Test 1: All checks pass

test_that("check_df passes with valid inputs", {
  expect_true(check_df(df_example, num_rows = 10, num_cols = 2,
                       col_names = c("name", "age"), col_types = c(name = "character", age = "integer"),
                       df_name = "df_example"))
})

# Test 2: Fails due to incorrect number of rows

test_that("check_df fails with incorrect num_rows", {
  expect_error(check_df(df_example, num_rows = 11, df_name = "df_example"))
})

# Test 3: Fails due to incorrect number of columns

test_that("check_df fails with incorrect num_cols", {
  expect_error(check_df(df_example, num_cols = 3, df_name = "df_example"))
})

# Test 4: Fails due to missing column name

test_that("check_df fails with incorrect col_names", {
  expect_error(check_df(df_example, col_names = c("name", "height"), df_name = "df_example"))
})

# Test 5: Fails due to incorrect column type

test_that("check_df fails with incorrect col_types", {
  expect_error(check_df(df_example, col_types = c(name = "numeric", age = "character"), df_name = "df_example"))
})

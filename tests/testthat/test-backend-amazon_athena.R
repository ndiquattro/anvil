trans <- function(x) {
  dbplyr::translate_sql({{x}}, con = dbplyr::simulate_dbi("amazon_athena"))
}

test_that("paste0 is translated", {
  expect_equal(trans(paste0(x, y)), dbplyr::sql("CONCAT(`x`, `y`)"))
  expect_equal(trans(paste0(x, y, z)), dbplyr::sql("CONCAT(`x`, `y`, `z`)"))
})

test_that("casting translates correctly", {
  expect_equal(trans(as.character("first_name")), dbplyr::sql("CAST('first_name' AS VARCHAR)"))
  expect_equal(trans(as.integer("123")), dbplyr::sql("CAST('123' AS INTEGER)"))
})

test_that("stringr functions are translated", {
  # str_detect
  expect_equal(trans(str_detect(x, y)), dbplyr::sql("REGEXP_LIKE(`x`, '`y`')"))
  expect_equal(trans(str_detect(x, y, negate = TRUE)), dbplyr::sql("NOT(REGEXP_LIKE(`x`, '`y`'))"))

  # str_remove_all
  expect_equal(trans(str_remove_all(x, y)), dbplyr::sql("REGEXP_REPLACE(`x`, `y`)"))

  # str_replace_all
  expect_equal(trans(str_replace_all(x, y, z)), dbplyr::sql("REGEXP_REPLACE(`x`, `y`, `z`)"))

  # str_starts
  expect_equal(trans(str_starts(x, y)), dbplyr::sql("REGEXP_LIKE(`x`, '^`y`')"))
  expect_equal(trans(str_starts(x, y, negate = TRUE)), dbplyr::sql("NOT(REGEXP_LIKE(`x`, '^`y`'))"))

  # str_ends
  expect_equal(trans(str_ends(x, y)), dbplyr::sql("REGEXP_LIKE(`x`, '`y`$')"))
  expect_equal(trans(str_ends(x, y, negate = TRUE)), dbplyr::sql("NOT(REGEXP_LIKE(`x`, '`y`$'))"))

  # str_extract(_all)
  expect_equal(trans(str_extract(x, y)), dbplyr::sql("REGEXP_EXTRACT(`x`, `y`)"))
  expect_equal(trans(str_extract_all(x, y)), dbplyr::sql("REGEXP_EXTRACT_ALL(`x`, `y`)"))
})

test_that("Data types are translated", {
  fake_con <- dbplyr::simulate_dbi("amazon_athena")

  test_obs <-
    list(
      bit64::as.integer64(123),
      TRUE,
      1L,
      1.5,
      factor("hey"),
      "goodbye",
      Sys.Date(),
      Sys.time()
    )

  expected_trans <-
    c("BIGINT",
      "BOOLEAN",
      "INTEGER",
      "DOUBLE",
      "STRING",
      "STRING",
      "DATE",
      "TIMESTAMP")

  trans <- db_data_type(fake_con, test_obs)

  expect_equal(trans, expected_trans)
  expect_error(db_data_type(fake_con, list(mtcars)))
})

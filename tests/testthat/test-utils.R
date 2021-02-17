test_that("connection is tested", {
  con <- NULL
  expect_error(check_con(con), "Con is NULL. Have you run av_connect()?")
})

test_that("quoter quotes correctly", {

  expect_equal(quote_name("data-science"), '"data-science"')
  expect_equal(quote_name('data-science'), '"data-science"')
})

with_mock_db({
  test_that("Has table works", {
    con <- DBI::dbConnect(odbc::odbc(), "athena")
    yes_table <- has_table(con, "data-science", "match_sample")
    not_table <- has_table(con, "data-science", "chocolate")

    expect_true(yes_table)
    expect_false(not_table)
  })
})

test_that("Full name parser works", {
  test1 <- parse_full_name("data-science.data_avail")
  test2 <- parse_full_name('"data-science".data_avail')

  expect_equal(test1[["schema"]], "data-science")
  expect_equal(test2[["schema"]], "data-science")

  expect_equal(test1[["table"]], "data_avail")
  expect_equal(test2[["table"]], "data_avail")
})

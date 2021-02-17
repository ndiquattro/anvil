setup({
  dittodb::start_mock_db()
})

teardown({
  dittodb::stop_mock_db()
})

con <- DBI::dbConnect(odbc::odbc(), "athena", dbms.name = "amazon_athena")

test_that("Table drop is requested", {
  res <- av_drop_table("anvil_test5", con = con)

  expect_equal(dim(res), c(0, 0))
})

setup({
  dittodb::start_mock_db()
})

teardown({
  dittodb::stop_mock_db()
})

con <- DBI::dbConnect(odbc::odbc(), "athena", dbms.name = "amazon_athena")

test_that("Table list is returned with full_name added", {
  suppressWarnings({  # dittodb issue
    tables <-
      av_ls_tables(con = con) %>%
      head(10) %>%
      dplyr::collect()
  })

  expect_equal(dim(tables), c(10, 5))
  expect_equal(names(tables), tolower(names(tables)))
})

# test_that("Pattern argument filters results", {
#   suppressWarnings({  # dittodb issue
#     res <-
#       av_ls_tables("data-science", con = con) %>%
#       dplyr::collect()
#   })
#
#   #expect_equal(dim(res), c(4, 5))
# })

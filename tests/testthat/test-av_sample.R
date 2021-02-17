test_that("Invalid pct values are caught", {
  expect_error(av_sample(x, 50), "pct must be between 0 and 1.")
  expect_error(av_sample(x, -1), "pct must be between 0 and 1.")
})


dittodb::with_mock_db({
  test_that("Query is created correctly", {
    con <- DBI::dbConnect(odbc::odbc(), "athena", dbms.name = "amazon_athena")

    suppressWarnings({  # dittodb issue
      rs <-
        av_gt("data-science", "graphite_sample", con = con) %>%
        av_sample(.1) %>%
        head(5) %>%
        dplyr::collect()
    })
    expect_equal(dim(rs), c(5, 36))
  })
})

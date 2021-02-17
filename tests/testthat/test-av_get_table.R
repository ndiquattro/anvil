setup({
  dittodb::start_mock_db()
})

teardown({
  dittodb::stop_mock_db()
})

con <- DBI::dbConnect(odbc::odbc(), "athena")

test_that("A table is returned correctly", {
  suppressWarnings({  # dittodb issue
    apps <-
      av_get_table("data", "appends", con = con) %>%
      head(10) %>%
      dplyr::collect()
  })

  expect_equal(names(apps), tolower(names(apps)))
})

test_that("Hyphenated schemas are quoted successfully", {
  suppressWarnings({  # dittodb issue
    df <-
      av_get_table("data-science", "match_sample", con = con) %>%
      head(10) %>%
      dplyr::collect()
  })

  expect_equal(dim(df), c(10, 4))
  expect_equal(names(df), tolower(names(df)))
})

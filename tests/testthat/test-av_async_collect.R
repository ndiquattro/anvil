turl <- "https://athena.us-east-2.amazonaws.com"

with_mock_api({
  test_that("Character query is POSTed", {
    tquery <- "SELECT * FROM data.appends"
    exp_body <- '.*\"QueryString\":\"SELECT \\* FROM data.appends.*\"'

    expect_POST(av_async_collect(tquery), turl, body=exp_body, fixed = FALSE)
  })

  test_that("tbl is POSTed", {
    tdf <- dbplyr::memdb_frame(mtcars)
    exp_body <- '.*\"QueryString\":\"SELECT \\*\\\\nFROM `dbplyr_001`\"'

    expect_POST(av_async_collect(tdf), turl, exp_body, fixed = FALSE)
  })
})

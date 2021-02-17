setup({
  dittodb::start_mock_db()
})

teardown({
  dittodb::stop_mock_db()
})

con <- DBI::dbConnect(odbc::odbc(), "athena", dbms.name = "amazon_athena")

test_that("Correct methods are available", {
  exp_meth <-
    c(
      "av_create_table.character",
      "av_create_table.data.frame",
      "av_create_table.tbl_amazon_athena",
      "av_create_table.tbl_sql"
    )
  has_meth <- as.character(methods(av_create_table))

  expect_equal(exp_meth, has_meth)
})

test_that("tbl is created correctly", {
  suppressWarnings({  # dittodb issue
    cre <- av_gt("rds-export-graphite", "graphite_person", con = con) %>%
      head(5) %>%
      av_create_table("anvil-test", con = con)
  })

  expect_equal(dim(cre), c(0, 0))
})

test_that("SQL character is created correctly", {
  suppressWarnings({  # dittodb issue
    cre <-
      av_create_table(
        "SELECT * FROM data.appends LIMIT 5", name = "anvil-test2",
        con = con
      )
  })

  expect_equal(dim(cre), c(0, 0))
})

test_that("Existing tables throw an error", {
  suppressWarnings({  # dittodb issue
    expect_error(av_create_table("SELECT * FROM data.appends LIMIT 10", "match_sample", con = con), "Table already exists.")
  })
})

with_mock_api({
  test_that("Dataframe is PUTted to aws", {
    turl <- "https://s3-us-east-2.amazonaws.com/alloy-datascience/anvil_upload/.+/anvil-test.parquet"

    expect_PUT(av_create_table(mtcars, "anvil-test", con = con), turl, fixed = FALSE)
  })
  test_that("Table is POSTed to aws", {
    tid <- "https:.*ClientRequestToken.*"
    expect_POST(av_create_table("select 1 as col", "anvil-test-async", async = TRUE, overwrite = FALSE, con = con),
                tid, fixed = FALSE)
    })
})

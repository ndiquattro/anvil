test_that("Unfound DSN throws an error", {
  expect_error(av_connect("aldkf"))
})

test_that("Missing connection_string and dsn arguments throws correct error", {
  exp_err <- "No credentials were provided. Set dsn or connection_string."

  expect_error(av_connect(), exp_err)
})

test_that("Setting both connection_string and dsn arguments throws error", {
  exp_err <- "Please only set dsn `or` connection_string."

  expect_error(av_connect(dsn = "db1", connection_string = "db=db1;"), exp_err)
})

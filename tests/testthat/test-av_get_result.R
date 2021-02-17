query_id <- "09b03bf1-f596-4cbe-8c04-06f380d4f3f1"

test_that("Only file or .f can be defined", {
  expect_error(
    av_get_result(query_id, file = tempfile(), .f = read.csv),
    "Only specify .f or file arguments."
  )
})

with_mock_api({
  test_that("File must have an extension", {
    expect_error(
      av_get_result(query_id, "test"),
      "Provided file does not have an extension. Maybe try .csv?"
    )
  })

  test_that("Failed queries cause an error", {
    expect_error(
      av_get_result("485316e3-931c-4288-b833-c8b3429e688d", "test.csv"),
      "Related query did not succeed. Status: "
    )
  })
})




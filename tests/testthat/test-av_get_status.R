turl <- "https://athena.us-east-2.amazonaws.com"

with_mock_api({
  test_that("Status request is made", {
    expect_POST(av_get_status("93e57ff1-4016-4495-861d-03d2890365a1"), turl, '{\"QueryExecutionId\":\"93e57ff1-4016-4495-861d-03d2890365a1\"}')
  })

  test_that("Only status is returned", {
    expect_equal(av_get_status("5431439b-9762-4ef1-96f4-1c73b7bc5fcf"), "SUCCEEDED")
  })
})

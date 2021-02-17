httptest::with_mock_api({

  test_that("bucket list is returned", {
    res <- av_ls_s3()

    expect_equal(dim(res), c(48, 2))
  })

  test_that("Bucket contents are returned", {
    res <- av_ls_s3("alloy-datascience")

    expect_equal(dim(res), c(41, 9))
  })

})

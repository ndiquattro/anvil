test_that("Non-multiples of 50 cause an error", {
  expect_error(av_ls_results(33), "n must be a multiple of 50.")
  expect_error(av_ls_results(76), "n must be a multiple of 50.")
  expect_error(av_ls_results(125), "n must be a multiple of 50.")
})

httptest::with_mock_api({

  test_that("full results pipe works", {
    full_results <- av_ls_results()

    expect_equal(dim(full_results), c(50, 10))
  })

})


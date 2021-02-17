#' Show documentation website
#'
#' Downloads the pkgdown site for anvil from S3 and opens in a browser.
#'
#' @export
show_docs <- function() {
  tdir <- file.path(tempdir(), "docs", "anvil")
  dir.create(tdir, recursive = TRUE)

  quiet(aws.s3::s3sync(tdir, "alloy-datascience", "docs/anvil/", direction = "download", verbose = FALSE))

  browseURL(file.path(tdir, "index.html"))
}

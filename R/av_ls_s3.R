#' List S3 buckets and contents
#'
#' Find S3 buckets you can access and the contents within a bucket.
#'
#' If `s3path` is left as `NULL` a tibble of available buckets will be returned.
#' When `s3path` is defined the contents at the given path will be returned.
#'
#' @param s3path s3:// URI or bucket/prefix style character.
#' @param max Number of results to return.
#' @param list_all When true will list entire key of each file.
#' @param ... Other parameters passed down to [`aws.s3::get_bucket_df()`].
#'
#' @return A tibble
#' @export
#'
#' @examples
#' # Calling with no arguments returns a list of buckets
#' av_ls_s3()
#'
#' # A bucket name will show top-level folders and root files
#' av_ls_s3("alloy-datascience")
#'
#' # Add slashes to dive deeper, like on Linux
#' av_ls_s3("alloy-datascience/models/")
#'
#' # s3:// url schemes can be present or not
#' av_ls_s3("s3://alloy-datascience/models/")
#'
#' # Number of responses are limited by default to keep responses snappy, adjust with max
#' av_ls_s3("alloy-datascience", max = 700, list_all = TRUE)
av_ls_s3 <- function(s3path = NULL, max = 100, list_all = FALSE, ...) {
  if (is.null(s3path)) {
    return(clean_s3_response(aws.s3::bucketlist()))
  }

  # Parse s3 path
  params <- parse_s3_loc(s3path)
  delim <- if (isFALSE(list_all)) "/" else NULL

  resp <- aws.s3::get_bucket(
    bucket = params[["bucket"]],
    prefix = params[["prefix"]],
    max = max,
    delimiter = delim,
    ...
  )

  com_pre <- attr(resp, "CommonPrefixes")

  dplyr::bind_rows(
    tibble::tibble(key = com_pre, is_prefix = TRUE),
    clean_s3_response(resp)
  ) %>%
  tidyr::replace_na(list(is_prefix = FALSE)) %>%
  dplyr::select(-dplyr::starts_with("owner_"), -storage_class) %>%
  dplyr::mutate(
    e_tag   = stringr::str_remove_all(e_tag, '"'),
    size    = as.numeric(size),
    size_mb = round(size / 1024 / 1024, 2),
    size_gb = round(size_mb / 1024, 2),
    uri = dplyr::if_else(is_prefix, NA_character_, paste0("s3://", bucket, "/", key))
  ) %>%
  dplyr::relocate(bucket, .after = dplyr::last_col()) %>%
  dplyr::arrange(dplyr::desc(is_prefix), dplyr::desc(last_modified))
}

clean_s3_response <- function(x) {
  clean_names <- function(x) {
    stringr::str_replace_all(x, "([A-Z])", "_\\1") %>%
    stringr::str_sub(start = 2) %>%
    stringr::str_to_lower()
  }

  tibble::as_tibble(x) %>%
    dplyr::rename_all(clean_names) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("date") | dplyr::contains("modified"), lubridate::ymd_hms))
}

parse_s3_loc <- function(x) {
  if (!stringr::str_starts(x, "s3://")) {
    x <- paste0("s3://", x)
  }

  xml2::url_parse(x) %>%
    dplyr::transmute(
      bucket = server,
      prefix = stringr::str_sub(path, start = 2)
    ) %>%
    as.list()
}

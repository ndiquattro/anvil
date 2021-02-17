#' Unpack a nested column
#'
#' Extract nested Athena columns into their own column.
#'
#' Note this function works on the database end only. For unpacking in memory data see [`tidyr::unnest()`].
#'
#' @param .tbl A tbl_sql.
#' @param nested_col The target nested column.
#' @param keys Character vector of desired keys from `nested_col`. All keys will be extracted if `NULL`.
#' @param suffix Character value to add to the end of extracted column names.
#' @param drop If `TRUE` drop the nested_col after unpacking.
#'
#' @return A tbl_sql.
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @export
#'
#' @examples
#' btable <-
#'   av_gt("sos", "address") %>%
#'   transmute(cass = residential_address_cass$components)
#'
#' # Extract all
#' av_unpack(btable, cass)
#'
#' # Extract city and zip
#' av_unpack(btable, cass, keys = c("city_name", "zipcode"))
#'
#' # Add suffix to avoid name conflicts or easier later selection
#' av_unpack(btable, cass, "zipcode", suffix = "_cass")
av_unpack <- function(.tbl, nested_col, keys = NULL, suffix = NULL, drop = TRUE) {
  nested_col <- rlang::ensym(nested_col)

  if (is.null(keys)) {
    # Pull first row to discover all possible keys
    keys <-
      .tbl %>%
      head(1) %>%
      dplyr::pull(nested_col) %>%
      stringr::str_sub(2, -2) %>%
      stringr::str_split(", ") %>%
      unlist() %>%
      stringr::str_extract("^.*(?=\\=)")
  }

  # Make expressions
  ekeys <-
    keys %>%
    purrr::set_names(paste0, suffix) %>%
    purrr::map(~rlang::expr((!! nested_col)[[!! .x]]))

  out <- rlang::exec(dplyr::mutate, .data = .tbl, !!! ekeys)

  if (drop) {
    out <- dplyr::select(out, -{{ nested_col }})
  }

  out
}

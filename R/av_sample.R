#' Randomly sample rows
#'
#' Sample a percentage of an Athena table
#'
#' This uses `TABLESAMPLE BERNOULLI()` under the hood on the current query
#' specified by the `tbl`. The number of rows returned is not deterministic.
#' That is, asking for .1 of a 1,000 row table may not be equal to 100 rows
#' every run.
#'
#' @param .tbl A tbl_sql.
#' @param pct Double. Desired proportion of full table to randomly return.
#'
#' @return A tbl_sql.
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @export
#'
#' @examples
#' # Sample 10% of Colorado
#' av_gt("data-science", "graphite_sample") %>%
#'   dplyr::filter(registration_state == "CO") %>%
#'   av_sample(.1)
av_sample <- function(.tbl, pct = 1) {
  if (pct > 1 | pct < 0) {
    stop("pct must be between 0 and 1.")
  }

  pct <- pct * 100  # Athena expects a 0 - 100 scale
  con <- .tbl$src$con

  out_sql <-
    dbplyr::build_sql(
      "SELECT * FROM ",
      dplyr::sql_subquery(con, dbplyr::sql_render(.tbl)),
      " TABLESAMPLE BERNOULLI (", pct, ")",
      "\n",
      con = con
    )

  dplyr::tbl(con, out_sql)
}

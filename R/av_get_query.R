#' Get results of a SQL query
#'
#' Executes a SQL statement provided as a character string on Athena.
#'
#' The query string is processed through [`dplyr::tbl()`]. This means it
#' initially returns a tibble-like preview of the query. To get the full results
#' you must call [`dplyr::collect()`] on the results, which will bring the data
#' into memory as a tibble. At this point the data could be converted to another
#' tabular data structure, if desired.
#'
#' @param query Character. A valid SQL statement.
#' @param collect Boolean. Automatically load results into memory.
#' @param con Object. Database connection to use. Defaults to most recent call
#'   of [`av_connect()`].
#'
#' @return A tbl.
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @family get data functions
#'
#' @export
#'
#' @examples
#' apps <- av_get_query("SELECT * FROM data.appends")
#'
#' # You need to quote correctly
#' samps <- av_get_query('SELECT alloy_id FROM "data-science".match_sample')
#'
#' # You can integrate with other dplyr functions
#' av_get_query('SELECT * FROM "rds-export-graphite".graphite_address') %>%
#'   dplyr::filter(state == "CA")
#'
#' # Pulling results and converting
#' av_get_query("SELECT * FROM data.appends") %>%
#'   dplyr::collect() %>%
#'   as.data.frame()
#'
#' # Auto collect
#' res <- av_get_query("SELECT * FROM data.appends LIMIT 10", collect = TRUE)
av_get_query <- function(query, collect = FALSE, con = getOption("anvil.con")) {
  check_con(con)

  results <- dplyr::tbl(con, dbplyr::sql(query))

  if (collect) {
    return(dplyr::collect(results))
  }

  results
}

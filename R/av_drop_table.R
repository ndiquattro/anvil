#' Drop a table from Athena
#'
#' Removes a table from Athena.
#'
#' @param name Character. Name of table to drop.
#' @param schema Character. Name of the schema belonging to the table.
#' @param con Object. Database connection to use. Defaults to most recent call
#'   of [av_connect()].
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @family table modifiers
#'
#' @export
#'
#' @examples
#' av_drop_table("anvil_test")
av_drop_table <- function(name, schema = "data-science", con = getOption("anvil.con")) {
  check_con(con)

  rp <- DBI::dbGetQuery(con, stringr::str_glue("DROP TABLE IF EXISTS `{schema}.{name}`"))

  invisible(rp)
}

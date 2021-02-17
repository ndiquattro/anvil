#' List tables and schema on Athena
#'
#' Queries `information_schema.tables` to help explore the database. Optionally
#' filter with a regular expression on the full_name column.
#'
#' @param pattern Character. Pattern to use as a filter. Accepts regular
#'   expressions.
#' @param con Object. Database connection to use. Defaults to most recent call
#'   of [av_connect()].
#'
#' @return A tbl.
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @family list functions
#'
#' @export
#'
#' @importFrom stringr str_detect
#'
#' @examples
#' # See all tables on athena
#' av_ls_tables() %>%
#'   print(n = Inf)
#'
#' # Find schema.table designations that contain 'alloy'
#' av_ls_tables(pattern = "alloy")
av_ls_tables <- function(pattern, con = getOption("anvil.con")) {
  check_con(con)

  base_table <-
    av_get_table("information_schema", "tables", con = con) %>%
    dplyr::mutate(full_name = paste0(.data$table_schema, ".", .data$table_name))


  if (!missing(pattern)) {
    return(dplyr::filter(base_table, str_detect(.data$full_name, pattern)))
  }

  base_table
}

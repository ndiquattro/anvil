#' Get Table
#'
#' Queries for data from a given schema tablename.
#'
#' @param schema Character. Name of desired schema.
#' @param table Character. Name of desired table.
#' @param full_name Character. Full name of a table in the form of `<schema>.<table>`.
#' @param con Object. Database connection to use. Defaults to most recent call
#'   of [av_connect()].
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
#' # Things are quotes automatically
#' av_get_table("rds-export-graphite", "graphite_address") # No extra quotes!
#'
#' # Run collect to bring a full dataset into memory
#' library(dplyr)
#' sample <-
#'   av_get_table("rds-export-graphite", "graphite_address") %>%
#'   head(20) %>% # Same as LIMIT 20
#'   dplyr::collect()
#'
#' # Results can be joined with other dplyr verbs for manipulation
#' av_get_table("rds-export-graphite", "graphite_address") %>%
#'   dplyr::filter(state == "CA") %>%
#'   dplyr::select(state, starts_with("address"))
#'
#' # Copy and paste a full name
#' av_gt(full_name = "data-science.data_avail")
av_get_table <- function(schema, table, full_name = NULL, con = getOption("anvil.con")) {
  check_con(con)

  # Parse full name if defined
  if (!is.null(full_name)) {
    parsed_name <- parse_full_name(full_name)
    schema <- parsed_name[["schema"]]
    table  <- parsed_name[["table"]]
  }

  dplyr::tbl(con, dbplyr::in_schema(schema, table))
}

#' @rdname av_get_table
#' @examples
#' av_gt("rds-export-graphite", "graphite_address")
#' @export
av_gt <- av_get_table

#' Connect to a remote database
#'
#' Creates a database connection from a given a Data Source Name (DSN) or ODBC
#' connection string.
#'
#' Only set the `dsn` or `connection_string` arguments. Add addional arguments
#' with `...` if needed. If using `connection_string` it should take the
#' following format: `"<name>=<value>;<name>=<value>"`.
#'
#' @param dsn Character that corresponds to a DSN defined in `~/.odbc.ini`.
#' @param connection_string Character. ODBC standard connection string.
#' @param set_default Boolean. Sets connection object as default for session. It
#'   is stored in options(anvil.con).
#' @param ... Additional ODBC name/value pairs. These will be joined with the
#'   other arguments.
#'
#' @return A connection object
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @family connection functions
#'
#' @export
#'
#' @examples
#' # For a DSN named "athena"
#' con <- av_connect("athena")
#'
#' # If you need a separate connection
#' con2 <- av_connect("other_db", set_default = FALSE)
av_connect <- function(dsn = NULL, connection_string = NULL, set_default = TRUE,
                       ...) {
  if (is.null(c(dsn, connection_string))) {
    stop("No credentials were provided. Set dsn or connection_string.")
  }

  if (!is.null(dsn) & !is.null(connection_string)) {
    stop("Please only set dsn `or` connection_string.")
  }

  con <-
    DBI::dbConnect(
      drv = odbc::odbc(),
      dsn = dsn,
      .connection_string = connection_string,
      dbms.name = "amazon_athena",
      ...
    )

  if (set_default) {
    options(list(anvil.con = con))
  }

  con
}

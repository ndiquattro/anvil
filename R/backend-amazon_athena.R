#' @importFrom dbplyr sql_translation
#' @method sql_translation amazon_athena
#' @keywords internal
#' @export
sql_translation.amazon_athena <- function(con) {
  dbplyr::sql_variant(
    base_athena_scalar,
    base_athena_agg,
    base_athena_win
  )
}

detector <- function(pre = NULL, post = NULL) {
  function(string, pattern, negate = FALSE) {
    pattern <- paste0(pre, pattern, post)

    if (negate) {
      return(dbplyr::sql_expr(NOT(REGEXP_LIKE(!!string, !!pattern))))
    }

    dbplyr::sql_expr(REGEXP_LIKE(!!string, !!pattern))
  }
}

base_athena_scalar <-
  dbplyr::sql_translator(
    .parent = dbplyr::base_odbc_scalar,

    # Base R Functions ----
    paste0       = dbplyr::sql_prefix("CONCAT"),
    as.character = dbplyr::sql_cast("VARCHAR"),
    as.integer   = dbplyr::sql_cast("INTEGER"),

    # Stringr Functions -----
    str_detect      = detector(),
    str_ends        = detector(post = "$"),
    str_extract     = dbplyr::sql_prefix("REGEXP_EXTRACT", 2),
    str_extract_all = dbplyr::sql_prefix("REGEXP_EXTRACT_ALL", 2),
    str_starts      = detector("^"),
    str_remove_all  = dbplyr::sql_prefix("REGEXP_REPLACE", 2),
    str_replace_all = dbplyr::sql_prefix("REGEXP_REPLACE", 3)
  )

base_athena_agg <-
  dbplyr::sql_translator(
    .parent = dbplyr::base_odbc_agg,

    cor    = dbplyr::sql_prefix("CORR"),
    cov    = dbplyr::sql_prefix("COVAR_SAMP"),
    sd     = dbplyr::sql_prefix("STDDEV_SAMP"),
    var    = dbplyr::sql_prefix("VAR_SAMP"),
    paste0 = dbplyr::sql_prefix("CONCAT")
  )

base_athena_win <-
  dbplyr::sql_translator(
    .parent = dbplyr::base_odbc_win,

    cor    = dbplyr::sql_prefix("CORR"),
    cov    = dbplyr::sql_prefix("COVAR_SAMP"),
    sd     = dbplyr::sql_prefix("STDDEV_SAMP"),
    var    = dbplyr::sql_prefix("VAR_SAMP"),
    paste0 = dbplyr::sql_prefix("CONCAT")
  )

#' Convert R data type to Athena
#'
#' @param con Athena connection
#' @param fields fields to type reference
#' @param ... ignored
#'
#' @importFrom dplyr db_data_type
#'
#' @method db_data_type amazon_athena
#'
#' @keywords internal
#' @export
db_data_type.amazon_athena <- function(con, fields, ...) {
  data_type <- function(x) {
    switch(
      class(x)[1],
      integer64 = "BIGINT",
      logical   = "BOOLEAN",
      integer   = "INTEGER",
      numeric   = "DOUBLE",
      factor    = "STRING",
      character = "STRING",
      Date      = "DATE",
      POSIXct   = "TIMESTAMP",
      stop("Can't map type ", paste(class(x), collapse = "/"),
           " to a supported database type.")
    )
  }
  vapply(fields, data_type, character(1))
}

#' @export
#' @importFrom dplyr db_desc
#' @method db_desc amazon_athena
db_desc.amazon_athena <- function(x) {
  info <- DBI::dbGetInfo(x)

  paste0(info$dbms.name, " [ODBC Driver ", info$driver.version, "]")
}

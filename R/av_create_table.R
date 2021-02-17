#' Create an Athena table
#'
#' Create a table from a tbl, SQL expression, or local data.frame.
#'
#' This function will take different paths to create a table based on the value
#' of `x`. When `x` is a tbl or character a "CREATE TABLE AS" query is generated
#' and executed. When `x` is a local data.frame, the data is written to parquet
#' locally using [arrow::write_parquet()], uploaded to S3, and a "CREATE
#' EXTERNAL TABLE" query is generated and executed. In this last case, the
#' location of the data on S3 is returned. See links in References for more.
#'
#' Note that partitioning is only supported for SQL and tbl_sql inputs. Athena
#' also requires the partitioning columns are "last" in the query.
#'
#' @param x A tbl, valid SQL character string, or data.frame.
#' @param name Character. Desired name of table.
#' @param schema Character. Desired schema.
#' @param partition_cols Character vector designating partition columns.
#' @param bucket_cols Character vector designating bucket columns. Note you must also define `bucket_count`.
#' @param bucket_count Interger designating the number of buckets (thus files) to create. Note you must also define `bucket_cols`.
#' @param overwrite Boolean. If `TRUE` will call [`av_drop_table()`] before
#'   attempting to create.
#' @param async Boolean. If `FALSE` will wait for the table to finish building.
#'   If `TRUE` will call [`av_async_collect()`] and return the Athena QueryExecutionId
#' @param con Object. Database connection to use. Defaults to most recent call
#'   of [av_connect()].
#'
#' @return If `x` is a data.frame then the location of the data on S3.
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @family table modifiers
#'
#' @references [CREATE TABLE AS
#'   Docs](https://docs.aws.amazon.com/athena/latest/ug/create-table-as.html)
#'
#'   [CREATE TABLE
#'   Docs](https://docs.aws.amazon.com/athena/latest/ug/create-table.html)
#'
#'   [Bucketing vs Partitioning](https://docs.aws.amazon.com/athena/latest/ug/bucketing-vs-partitioning.html)
#'
#'   [Specifying file size with buckets](https://aws.amazon.com/premiumsupport/knowledge-center/set-file-number-size-ctas-athena/?)
#'
#' @export
#'
#' @examples
#' # From a tbl
#' av_gt("rds-export-graphite", "graphite_person") %>%
#'   head(50) %>%
#'   av_create_table("anvil_test")
#'
#' # From SQL
#' tsql <- 'SELECT * FROM "rds-export-graphite".graphite_person LIMIT 49'
#' av_create_table(tsql, name = "anvil_test2")
#'
#' # Uploading a data.frame
#' av_create_table(mtcars, name = "anvil_test3")
av_create_table <- function(x,
                            name,
                            schema = "data-science",
                            partition_cols = NULL,
                            bucket_cols = NULL,
                            bucket_count = NULL,
                            overwrite = FALSE,
                            async = FALSE,
                            con = getOption("anvil.con")) {
  if (overwrite) {
    av_drop_table(name = name, schema = schema, con = con)
  }

  if (!overwrite && has_table(con, schema, name)) {
    stop("Table already exists.")
  }

  if (length(c(bucket_cols, bucket_count)) == 1) {
    stop("You must define bucket_cols AND bucket_count or neither.")
  }

  UseMethod("av_create_table", x)
}

#' @export
av_create_table.tbl_amazon_athena <- function(x,
                                              name,
                                              schema = "data-science",
                                              partition_cols = NULL,
                                              bucket_cols = NULL,
                                              bucket_count = NULL,
                                              overwrite = FALSE,
                                              async = FALSE,
                                              con = getOption("anvil.con")) {
  # Quote schema and table to avoid Athena errors
  schema <- quote_name(schema)
  name <- quote_name(name)

  # Define base parameters
  with_params <- list(
    format = "PARQUET",
    parquet_compression = "SNAPPY"
  )

  # Partitions
  if (!is.null(partition_cols)) {
    array_vals <-
      partition_cols %>%
      sQuote(q = FALSE) %>%
      paste(collapse = ", ")

    with_params["partitioned_by"] <- stringr::str_glue("ARRAY[{array_vals}]")

    # Put partition columns at the end of the select statement
    x <- dplyr::select(x, -dplyr::all_of(partition_cols), dplyr::everything())
  }

  # Buckets
  if (!is.null(bucket_cols)) {
    array_vals <-
      bucket_cols %>%
      sQuote(q = FALSE) %>%
      paste(collapse = ", ")

    with_params["bucketed_by"] <- stringr::str_glue("ARRAY[{array_vals}]")
    with_params["bucket_count"] <- as.integer(bucket_count)
  }

  csql <- dbplyr::sql_render(x, con)

  cq <- stringr::str_glue(
    "CREATE TABLE {schema}.{name} WITH ({make_params(with_params)}) AS {csql}"
  )

  if (async) {
    return(av_async_collect(cq))
  }

  invisible(DBI::dbGetQuery(con, cq))
}

#' @export
av_create_table.tbl_sql <- av_create_table.tbl_amazon_athena

#' @export
av_create_table.character <- function(x,
                                      name,
                                      schema = "data-science",
                                      partition_cols = NULL,
                                      bucket_cols = NULL,
                                      bucket_count = NULL,
                                      overwrite = FALSE,
                                      async = FALSE,
                                      con = getOption("anvil.con")) {

  # Quote schema and table to avoid Athena errors
  schema <- quote_name(schema)
  name <- quote_name(name)

  # Define base parameters
  with_params <- list(
    format = "PARQUET",
    parquet_compression = "SNAPPY"
  )

  # Partitions
  if (!is.null(partition_cols)) {
    array_vals <-
      partition_cols %>%
      sQuote(q = FALSE) %>%
      paste(collapse = ", ")

    with_params["partitioned_by"] <- stringr::str_glue("ARRAY[{array_vals}]")
  }

  # Buckets
  if (!is.null(bucket_cols)) {
    array_vals <-
      bucket_cols %>%
      sQuote(q = FALSE) %>%
      paste(collapse = ", ")

    with_params["bucketed_by"] <- stringr::str_glue("ARRAY[{array_vals}]")
    with_params["bucket_count"] <- as.integer(bucket_count)
  }

  cq <- stringr::str_glue(
    "CREATE TABLE {schema}.{name} WITH ({make_params(with_params)}) AS {x}"
  )

  if (async) {
    return(av_async_collect(cq))
  }

  invisible(DBI::dbGetQuery(con, cq))

}

#' @export
av_create_table.data.frame <- function(x,
                                       name,
                                       schema = "data-science",
                                       partition_cols = NULL,
                                       bucket_cols = NULL,
                                       bucket_count = NULL,
                                       overwrite = FALSE,
                                       async = FALSE,
                                       con = getOption("anvil.con")) {
  check_con(con)

  if (!is.null(partition_cols)) {
    stop("Partitioned tables not supported for data.frame uploads.")
  }

  if (!is.null(bucket_cols)) {
    stop("Bucketed tables not supported for data.frame uploads.")
  }

  # Upload parquet file to S3
  bkt <- "alloy-datascience"
  obname <- paste0("anvil_upload/", basename(tempfile("")), "/", name, ".parquet")

  aws.s3::s3write_using(
    x,
    FUN = arrow::write_parquet,
    object = obname,
    bucket = bkt,
    opts = list(multipart = TRUE)
  )

  s3loc <- paste0("s3://", bkt, "/", dirname(obname), "/")

  # Get Data types
  db_types <- dplyr::db_data_type(con, x)

  # Create SQL query to create table
  csql <- stringr::str_glue(
    "CREATE EXTERNAL TABLE IF NOT EXISTS `{schema}`.{name} (",
    paste(paste0("`", names(db_types), "`"), db_types, collapse = ", "),
    ")",
    " ROW FORMAT SERDE 'org.apache.hadoop.hive.ql.io.parquet.serde.ParquetHiveSerDe'",
    " WITH SERDEPROPERTIES ('serialization.format' = '1')",
    " LOCATION '{s3loc}'"
  )

  if (async) {
    return(list(s3loc, av_async_collect(csql)))
  }

  DBI::dbGetQuery(con, csql)
  s3loc
}

make_params <- function(x) {
  not_array <-
    purrr::partial(
      stringr::str_detect,
      pattern = "ARRAY",
      negate = TRUE
    )

  x <- purrr::map_if(x, ~not_array(.x) & !is.numeric(.x), sQuote, q = FALSE)

  paste(names(x), x, sep = " = ", collapse = ", ")
}

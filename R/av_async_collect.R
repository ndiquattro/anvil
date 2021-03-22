#' Execute Athena query asynchronously
#'
#' Uses AWS API to send a query to Athena for execution. Returns the query
#' execution id for later retrieval of the results using [av_get_result()].
#'
#' @param x A tbl or valid SQL character string.
#' @param workgroup A string indicating the workgroup to use in the request.
#'
#' @return A query execution ID
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @family get data functions
#'
#' @seealso
#' * [av_get_result()] retrieves results from Athena using a execution ID.
#' * [av_get_status()] returns the status of a query based on execution ID.
#'
#' @export
#'
#' @examples
#' qid <-
#'   av_gt("data", "appends") %>%
#'   dplyr::filter(!is.na(household_id)) %>%
#'   head(50) %>%
#'   av_async_collect()
#' av_get_result(qid, .f = read.csv)
#'
#' qid <- av_async_collect("SELECT * FROM data.appends LIMIT 50")
#' av_get_result(qid, .f = read.csv)
av_async_collect <- function(x, workgroup = "data-science") {
  UseMethod("av_async_collect", x)
}

#' @export
av_async_collect.character <- function(x, workgroup = "data-science") {
  aws.athena::start_athena_execution(x, workgroup)$QueryExecutionId
}

#' @export
av_async_collect.tbl_sql <- function(x, workgroup = "data-science") {
  trans <- dbplyr::sql_render(x, x$src$con)
  aws.athena::start_athena_execution(trans, workgroup)$QueryExecutionId
}

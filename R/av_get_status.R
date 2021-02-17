#' Check Athena query status
#'
#' Query for the status of an Athena query.
#'
#' Returns values could be "SUCCEEDED", "FAILED", "CANCELLED", or "RUNNING".
#'
#' @param query_id Character. Query Execution ID to check.
#'
#' @return Character.
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @family get data functions
#'
#' @seealso
#' * [av_async_collect()] returns an execution ID before a query completes.
#' * [av_get_status()] returns the status of a query based on execution ID.
#'
#' @export
#'
#' @examples
#' av_get_status("8ba4e41e-3ad4-486c-a351-e8b62b1fd3f2")  # Succeeded
#' av_get_status("485316e3-931c-4288-b833-c8b3429e688d")  # Failed
av_get_status <- function(query_id) {
  aws.athena::get_athena_execution(query_id)$QueryExecutionDetail$Status$State
}

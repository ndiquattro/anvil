#' Get previously executed Athena query
#'
#' Either save a local copy of query results from S3 or read into memory.
#'
#' When defining `.f` a local copy is still temporarily saved to a tmp
#' directory. You may want to change the location of where R makes tempfiles if
#' this will fill your instance's space. See `help("tempdir")` details.
#'
#' @param query_id Character. Athena query id from the history page on the
#'   console.
#' @param file Character. File name and location to save local copy.
#' @param .f Function. A function that will read results into memory. Likely a
#'   csv-reader.
#' @param ... arguments supplied to `.f`.
#'
#' @return The saved file path or results of the provided function.
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
#' # Save local file
#' tq <- av_get_result("09b03bf1-f596-4cbe-8c04-06f380d4f3f1", "/data/test.csv")
#' read.csv(tq)
#'
#' # Load data into memory -- No saving
#' av_get_result("09b03bf1-f596-4cbe-8c04-06f380d4f3f1", .f = read.csv)
#'
#' # Alternately, load data into memory with fread
#' av_get_result("09b03bf1-f596-4cbe-8c04-06f380d4f3f1", .f = fread, na.strings = "NA")
#'
av_get_result <- function(query_id, file, .f, ...) {
  if (!missing(file) & !missing(.f)) {
    stop("Only specify .f or file arguments.")
  }

  b <- "alloy-athena-output-data-science"
  qid <- paste0(query_id, ".csv")

  id_status <- av_get_status(query_id)
  if (id_status != "SUCCEEDED") {
    stop(paste0("Related query did not succeed. Status: ", id_status))
  }

  if (missing(.f)) {
    if (tools::file_ext(file) == "") {
      stop("Provided file does not have an extension. Maybe try .csv?")
    }

    aws.s3::save_object(qid, b, file)
    return(invisible(file))
  }

  aws.s3::s3read_using(.f, ..., object = qid, bucket = b)
}

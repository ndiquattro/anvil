#' List previous Athena queries
#'
#' Explore recent Athena queries
#'
#' Beware that larger values of n may result in incomplete results due to rate
#' limiting from AWS.
#'
#' @param n Integer. Number of queries to return. Should be in multiples of 50.
#'
#' @return A tibble.
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @family list functions
#'
#' @export
#'
#' @examples
#' av_ls_results(150)
av_ls_results <- function(n = 50) {
  if (n %% 50 != 0) {
    stop("n must be a multiple of 50.")
  }

  df_cols <-
    c(
      "QueryExecutionContext",
      "ResultConfiguration",
      "Statistics",
      "Status"
    )

  get_ids(n) %>%
    purrr::map(aws.athena::get_athena_execution) %>%
    purrr::map("QueryExecutions") %>%
    purrr::map_df(clean_df, cols = df_cols) %>%
    dplyr::mutate(
      gb_scanned = .data$datascannedinbytes / 1024^3,
      total_time_seconds = .data$totalexecutiontimeinmillis / 1000,
      submitted_at = lubridate::as_datetime(.data$submissiondatetime),
      completed_at = lubridate::as_datetime(.data$completiondatetime),
    ) %>%
    dplyr::select(
      id = .data$queryexecutionid,
      .data$state,
      .data$query,
      .data$database,
      .data$workgroup,
      output_location = .data$outputlocation,
      # encryption_option = encryptionconfiguration,
      .data$gb_scanned,
      .data$total_time_seconds,
      .data$submitted_at,
      .data$completed_at,
      #notes = statechangereason
    ) %>%
    dplyr::arrange(dplyr::desc(.data$completed_at))
}

# Recursive function for gathering IDs
get_ids <- function(total_n = 50, token = NULL, ids = list()) {
  calls <- ceiling(total_n / 50)

  if (length(ids) == calls) {
    return(ids)
  }

  call <- aws.athena::list_athena_executions(token = token, workgroup = "data-science")

  ids <- append(ids, list(call$QueryExecutionIds))
  token <- call$NextToken

  get_ids(total_n, token, ids)
}

# Clean data frames provides in response. Main concern is rectangling data.frame
# columns
clean_df <- function(df, cols) {
  df_cols_df <- purrr::map(cols, ~ purrr::pluck(df, .))

  # Nasty fix for when query was on encrypted results
  if (!is.null(df_cols_df[[2]]$EncryptionConfiguration)) {
    df_cols_df[[2]] <-
      within(df_cols_df[[2]], {
        EncryptionConfiguration <- EncryptionConfiguration$EncryptionOption
      })
  }

  df_cols_df <- purrr::reduce(df_cols_df, dplyr::bind_cols)

  df %>%
    tibble::as_tibble() %>%
    dplyr::select(-dplyr::one_of(cols)) %>%
    dplyr::bind_cols(df_cols_df) %>%
    dplyr::rename_all(tolower)
}

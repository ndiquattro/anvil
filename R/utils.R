# DB connection checks
check_con <- function(con) {
  if (is.null(con)) {
    stop("Con is NULL. Have you run av_connect()?")
  }
}

# Quotes table/schema names to be compatible with Athena
quote_name <- function(x) {
  dQuote(x, q = FALSE)
}

# Parse full name of a db table
parse_full_name <- function(x) {
  stringr::str_split(x, "\\.") %>%
    unlist() %>%
    stringr::str_remove_all('"') %>%
    purrr::set_names(c("schema", "table"))
}

# Check if table exists
has_table <- function(con, schema, table) {
  DBI::dbExistsTable(con, DBI::Id(schema = schema, table = table))
}

# Build Local docs - will delete when R session ends
gen_docs <- function(examples = FALSE, ...) {
  if (!"pkgdown" %in% utils::installed.packages()) {
    stop("You need pkgdown. install.packages('pkgdown')")
  }

  tdir <- file.path(tempdir(), "docs")
  pkgdown::build_site(
    examples = examples,
    preview = FALSE,
    override = list(destination = tdir),
    ...
  )

  aws.s3::s3sync(tdir, "alloy-datascience", "docs/anvil/", direction = "upload")
}

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

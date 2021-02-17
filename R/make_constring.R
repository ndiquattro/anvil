#' Make a ODBC connection string from a DSN file
#'
#' Converts a defined Data Source Name (DSN) into a connection string for use as an ENV.
#'
#' @param dsn Name of target DSN.
#' @param dsn_file Path to ODBC DSN file.
#'
#' @return A connection string.
#'
#' @author Nicholas DiQuattro \email{nicholas@alloy.us}
#'
#' @export
#'
#' @examples
#' # Set env
#' Sys.setenv(ATHENA_CON = make_constring("athena"))
#'
#' av_connect(connection_string = Sys.getenv("ATHENA_CON"))
make_constring <- function(dsn, dsn_file = "/etc/odbc.ini") {
  # Read file
  ini <- read_ini(dsn_file)

  target <- ini[[dsn]]
  paste(names(target), target, sep = "=", collapse = ";")
}


# Stolen from https://stackoverflow.com/questions/54302007/how-to-extract-data-from-ini-file-in-r
read_ini = function(fn) {
  blank = "^\\s*$"
  header = "^\\[(.*)\\]$"
  key_value = "^.*=.*$"

  extract = function(regexp, x) regmatches(x, regexec(regexp, x))[[1]][2]
  lines = readLines(fn)
  ini = list()
  for (l in lines) {
    if (grepl(blank, l)) next
    if (grepl(header, l)) {
      section = extract(header, l)
      ini[[section]] = list()
    }
    if (grepl(key_value, l)) {
      kv = strsplit(l, "\\s*=\\s*")[[1]]
      ini[[section]][[kv[1]]] = kv[2]
    }
  }
  ini
}

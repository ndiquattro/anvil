% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/av_ls_tables.R
\name{av_ls_tables}
\alias{av_ls_tables}
\title{List tables and schema on Athena}
\usage{
av_ls_tables(pattern, con = getOption("anvil.con"))
}
\arguments{
\item{pattern}{Character. Pattern to use as a filter. Accepts regular
expressions.}

\item{con}{Object. Database connection to use. Defaults to most recent call
of \code{\link[=av_connect]{av_connect()}}.}
}
\value{
A tbl.
}
\description{
Queries \code{information_schema.tables} to help explore the database. Optionally
filter with a regular expression on the full_name column.
}
\examples{
# See all tables on athena
av_ls_tables() \%>\%
  print(n = Inf)

# Find schema.table designations that contain 'alloy'
av_ls_tables(pattern = "alloy")
}
\seealso{
Other list functions: 
\code{\link{av_ls_results}()}
}
\author{
Nicholas DiQuattro \email{nicholas@alloy.us}
}
\concept{list functions}

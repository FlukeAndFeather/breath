% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_prh.R
\name{import_prh}
\alias{import_prh}
\title{Import a PRH into the CATS database}
\usage{
import_prh(prh, cats_db, overwrite = FALSE, verbose = FALSE)
}
\arguments{
\item{prh}{a PRH object}

\item{cats_db}{connection to CATS database (see \code{\link[DBI]{dbConnect}})}

\item{overwrite}{overwrite deployment in database? (FALSE by default)}

\item{verbose}{log progress to console? (FALSE by default)}
}
\value{
TRUE on success
}
\description{
Import a PRH into the CATS database
}
\examples{
bw180905_53 <- read_prh_nc("bw180905-53", "/Volumes/COPYCATSdat/CATS/")
db_path <- "/Volumes/COPYCATSdat/CATS/cats.sqlite"
cats_db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
import_prh(bw180905_53, cats_db, verbose = TRUE)
DBI::dbDisconnect(cats_db)
}

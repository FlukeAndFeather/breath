% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_prh.R
\name{read_prh_db}
\alias{read_prh_db}
\title{Read a PRH from the CATS database}
\usage{
read_prh_db(depid, cats_db)
}
\arguments{
\item{depid}{deployment identifier e.g. bw180827-46}

\item{cats_db}{connection to CATS database (see \code{\link[DBI]{dbConnect()}})}
}
\value{
a PRH object
}
\description{
Read a PRH from the CATS database
}
\examples{
db_path <- "/Volumes/COPYCATSdat/CATS/cats.sqlite"
cats_db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
mn190228_42_db <- read_prh_db("mn190228-42", cats_db)
DBI::dbDisconnect(cats_db)
}

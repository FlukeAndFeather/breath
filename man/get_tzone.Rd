% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prh.R
\name{get_tzone}
\alias{get_tzone}
\title{Get timezone of deployment}
\usage{
get_tzone(x)
}
\arguments{
\item{x}{either a PRH or a data.frame}
}
\value{
timezone of deployment e.g. "Etc/GMT-3"
}
\description{
Get timezone of deployment
}
\examples{
bw180905_53 <- read_prh_nc("bw180905-53", "/Volumes/COPYCATSdat/CATS/")
get_tzone(bw180905_53)
}

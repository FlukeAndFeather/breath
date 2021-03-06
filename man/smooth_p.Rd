% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{smooth_p}
\alias{smooth_p}
\title{Smooth pressure}
\usage{
smooth_p(x, t)
}
\arguments{
\item{x}{depth/time record}

\item{t}{time window (in seconds) to smooth over}
}
\value{
a depth/time record with smoothed pressure
}
\description{
Uses a running mean to smooth out noise in pressure
}

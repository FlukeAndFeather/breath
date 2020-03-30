#' Dive data of a blue whale
#'
#' Data courtesy James Fahlbusch, deployment bm181021. Decimated to 1 Hz with
#' breaths and dives identified.
#'
#' @format A list with three elements:
#' \describe{
#'   \item{data}{tibble of data,
#'   see below}
#'   \item{lunge_dt}{time of lunges, POSIXct}
#'   \item{fs}{sampling
#'   rate, Hz}
#' }
#'
#' The [data] element is a tibble with five variables:
#' \describe{
#'   \item{dt}{datetime, (POSIXct)}
#'   \item{p}{depth (originally pressure), (m)}
#'   \item{diveid}{positive for dives and negative for post-dive interval, numeric}
#'   \item{is_breath}{logical}
#'   \item{ibi}{inter-breath interval, (s) (NA unless a breath)}
#' }
"bm181021_dives"

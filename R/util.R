#' Convert MATLAB date numbers to POSIXct
#'
#' @param dn Vector of MATLAB date numbers (numeric)
#' @param tz Time zone (character)
#'
#' @return POSIXct vector
#' @export
#'
#' @examples
#'
#' # "2020-01-27 19:12:00 UTC"
#' dn_to_posix(737817.8)
dn_to_posix <- function(dn, tz = "UTC") {
  as.POSIXct((dn - 719529) * 86400, origin = "1970-01-01", tz = tz)
}

#' Smooth pressure
#'
#' Uses a running mean to smooth out noise in pressure
#'
#' @param x depth/time record
#' @param t time window (in seconds) to smooth over
#'
#' @return a depth/time record with smoothed pressure
#' @export
smooth_p <- function(x, t) {
  width <- x$fs * t
  x$data$p <- RcppRoll::roll_mean(x$data$p, n = width, fill = NA)
  x
}

count_lunges <- function(x, t) {
  sum(x$lunge_dt >= t[1] & x$lunge_dt <= t[length(t)])
}

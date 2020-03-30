#' Read deployment from ncdf file
#'
#' @param path File path to ncdf file
#' @param tz Time zone of deployment
#'
#' @return list with depth-time tibble (dt, p), vector of lunge times, and
#'   sampling rate (fs)
#' @export
read_deployment <- function(path, tz = "UTC") {
  # Read data and metadata
  ncdata <- RNetCDF::open.nc(path)
  dn <- RNetCDF::var.get.nc(ncdata, "DN")
  p <- RNetCDF::var.get.nc(ncdata, "P")
  lunges <- tryCatch(RNetCDF::var.get.nc(ncdata, "Lunges"),
                     error = function(e) NULL)
  fs <- RNetCDF::att.get.nc(ncdata, "P", "sampling_rate")

  # Convert DN to POSIXct
  dt <- dn_to_posix(dn, tz)

  # Read lunges
  if (is.null(lunges)) {
    lunge_dt <- NULL
  } else {
    lunge_dt <- as.POSIXct(lunges,
                           tz = tz,
                           origin = dt[1])
  }

  # Return result
  list(
    data = tibble(dt, p),
    lunge_dt = lunge_dt,
    fs = fs
  )
}

#' Decimate a record
#'
#' @param x a depth/time record
#' @param new_fs sampling frequency to decimate to
#'
#' @return a record with the new sampling frequency
#' @export
decimate <- function(x, new_fs) {
  old_fs <- x$fs
  if (old_fs %% new_fs != 0) {
    stop("new_fs is not a factor of original sampling frequency")
  }

  x$data <- slice(x$data, seq(1, nrow(x$data), by = old_fs / new_fs))
  x$fs <- new_fs
  x
}

#' Read depth/time record from ncdf file
#'
#' @param path File path to ncdf file
#' @param tz Time zone of deployment
#'
#' @return list with depth-time tibble (dt, p) and sampling rate (fs)
#' @export
read_depth <- function(path, tz = "UTC") {
  # Read data and metadata
  ncdata <- RNetCDF::open.nc(path)
  dn <- RNetCDF::var.get.nc(ncdata, "DN")
  p <- RNetCDF::var.get.nc(ncdata, "P")
  fs <- RNetCDF::att.get.nc(ncdata, "P", "sampling_rate")

  # Convert DN to POSIXct
  dt <- dn_to_posix(dn, tz)

  # Return result
  list(
    data = dplyr::tibble(dt, p),
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

  x$data <- dplyr::slice(x$data, seq(1, nrow(x$data), by = old_fs / new_fs))
  x$fs <- new_fs
  x
}

#' Get timezone of deployment
#'
#' @param x either a PRH or a data.frame
#'
#' @return timezone of deployment e.g. "Etc/GMT-3"
#' @export
#'
#' @examples
#' bw180905_53 <- read_prh_nc("bw180905-53", "/Volumes/COPYCATSdat/CATS/")
#' get_tzone(bw180905_53)
get_tzone <- function(x) {
  UseMethod("get_tzone")
}
get_tzone.data.frame <- function(metadata) {
  stopifnot("dephist_device_tzone" %in% colnames(metadata))
  sprintf("Etc/GMT%+d", -metadata$dephist_device_tzone)
}

#' PRH metadata accessors
#'
#' @param prh a PRH object
#'
#' @return PRH metadata e.g. the deployment identifier (`get_depid()`) or timezone (`get_tzone()`)
#'
#' @examples
#' bw180905_53 <- read_prh_nc("bw180905-53", "/Volumes/COPYCATSdat/CATS/")
#' get_depid(bw180905_53)
#' get_tzone(bw180905_53)
get_depid <- function(prh) {
  stopifnot(is_prh(prh))
  attr(prh, "metadata")$depid
}
#' @rdname get_depid
get_fs <- function(prh) {
  stopifnot(is_prh(prh))
  attr(prh, "metadata")$fs
}
#' @rdname get_depid
get_tzone.prh <- function(prh) {
  metadata <- attr(prh, "metadata")
  sprintf("Etc/GMT%+d", -metadata$dephist_device_tzone)
}

is_prh <- function(x) {
  "prh" %in% class(x)
}

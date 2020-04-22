#' Find a PRH in NetCDF format
#'
#' @param depid deployment identifier e.g. bw180827-46
#' @param cats_path location of CATS folder
#'
#' @return absolute path to NetCDF PRH file
#' @export
#'
#' @examples
#' find_nc("bw180827-46", "/Volumes/COPYCATSdat/CATS/")
find_nc <- function(depid, cats_path) {
  stopifnot(is.character(depid) && length(depid) == 1)
  stopifnot(is.character(cats_path) && length(cats_path) == 1)

  tag_path <- fs::dir_ls(fs::path_join(c(cats_path, "tag_data")),
                         regexp = glue::glue("{depid}"))
  stopifnot(length(tag_path) == 1)

  nc_path <- fs::dir_ls(tag_path, regexp = ".nc$")
  stopifnot(length(nc_path) == 1)

  nc_path
}

#' Read a PRH from NetCDF format
#'
#' @param depid deployment identifier e.g. bw180827-46
#' @param cats_path location of CATS folder
#'
#' @return a PRH object
#' @export
#'
#' @examples
#' bw180905_53 <- read_prh_nc("bw180905-53", "/Volumes/COPYCATSdat/CATS/")
read_prh_nc <- function(depid, cats_path) {
  stopifnot(is.character(depid) && length(depid) == 1)
  stopifnot(is.character(cats_path) && length(cats_path) == 1)

  # Locate NetCDF
  nc_path <- find_nc(depid, cats_path)
  stopifnot(length(nc_path) == 1)

  prh_nc <- ncdf4::nc_open(nc_path)
  metadata <- as_tibble(ncdf4::ncatt_get(prh_nc, 0))
  fs <- ncdf4::ncatt_get(prh_nc, "P", "sampling_rate")$value
  metadata$fs <- fs
  tz <- get_tzone(metadata)

  # read variables
  dn <- ncdf4::ncvar_get(prh_nc, "DN")
  dt <- dn_to_posix(dn, tz)
  p0 <- ncdf4::ncvar_get(prh_nc, "P")
  pitch0 <- ncdf4::ncvar_get(prh_nc, "pitch")
  roll0 <- ncdf4::ncvar_get(prh_nc, "roll")
  head0 <- ncdf4::ncvar_get(prh_nc, "head")
  speed0 <- ncdf4::ncvar_get(prh_nc, "speedJJ")
  speed0[is.nan(speed0)] <- NA
  A <- ncdf4::ncvar_get(prh_nc, "Aw")
  M <- ncdf4::ncvar_get(prh_nc, "Mw")
  G <- ncdf4::ncvar_get(prh_nc, "Gw")
  pos <- ncdf4::ncvar_get(prh_nc, "POS") %>%
    as_tibble() %>%
    purrr::set_names(c("secs", "lon", "lat"))

  # Create tibble (and smooth some variables)
  dep_start <- metadata$dephist_deploy_datetime_start %>%
    lubridate::dmy_hms(tz = "UTC") %>%
    lubridate::with_tz(tz)
  dep_end <- metadata$dephist_deploy_datetime_end %>%
    lubridate::dmy_hms(tz = "UTC") %>%
    lubridate::with_tz(tz)
  roll_mean <- function(x, n) {
    result <- RcppRoll::roll_mean(x, n, fill = NA)
    result[is.na(result)] <- x[is.na(result)]
    result
  }
  result <- tibble(dt,
                   p0, pitch0, roll0, head0, speed0,
                   A, M, G) %>%
    filter(between(dt, dep_start, dep_end)) %>%
    mutate(secs = round(as.numeric(dt - dt[1], unit = "secs"), digits = 3)) %>%
    mutate_at(
      vars(p0:speed0),
      list(smooth = ~ roll_mean(.x, n = fs * 2))
    ) %>%
    rename_at(
      vars(ends_with("smooth")),
      ~ substr(.x, 1, nchar(.x) - nchar("0_smooth"))
    ) %>%
    left_join(pos, by = "secs") %>%
    select(dt, secs, p:speed, lon:lat, A:G, p0:speed0)

  dimnames(result$A) <- list(NULL, c("x", "y", "z"))
  dimnames(result$M) <- list(NULL, c("x", "y", "z"))
  dimnames(result$G) <- list(NULL, c("x", "y", "z"))

  # Add metadata
  attr(result, "class") <- c("prh", "data.frame")
  attr(result, "metadata") <- metadata

  result
}

#' Read a PRH from the CATS database
#'
#' @param depid deployment identifier e.g. bw180827-46
#' @param cats_db connection to CATS database (see \code{\link[DBI]{dbConnect()}})
#'
#' @return a PRH object
#' @export
#'
#' @examples
#' db_path <- "/Volumes/COPYCATSdat/CATS/cats.sqlite"
#' cats_db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
#' mn190228_42_db <- read_prh_db("mn190228-42", cats_db)
#' DBI::dbDisconnect(cats_db)
read_prh_db <- function(depid, cats_db) {
  metadata <- tbl(cats_db, "tag_guide") %>%
    filter(depid == !!depid) %>%
    collect()
  if (nrow(metadata) == 0)
    stop(paste("depid not found:", depid))
  prh <- tbl(cats_db, "prh") %>%
    filter(depid == !!depid) %>%
    collect()

  # Matrix columns
  prh$A <- with(prh, as.matrix(cbind(A1, A2, A3)))
  prh$M <- with(prh, as.matrix(cbind(M1, M2, M3)))
  prh$G <- with(prh, as.matrix(cbind(G1, G2, G3)))
  dimnames(prh$A) <- list(NULL, c("x", "y", "z"))
  dimnames(prh$M) <- list(NULL, c("x", "y", "z"))
  dimnames(prh$G) <- list(NULL, c("x", "y", "z"))

  # POSIX column
  prh$dt <- as.POSIXct(
    prh$dt,
    origin = "1970-01-01",
    tz = get_tzone(metadata)
  )

  prh <- select(prh, dt, secs, p:speed, lon:lat, A:G, p0:speed0)

  attr(prh, "class") <- c("prh", "data.frame")
  attr(prh, "metadata") <- metadata
  prh
}

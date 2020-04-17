## code to prepare `feat` dataset goes here

### THIS IS A TEMPORARY BAND-AID AND SHOULD BE REPLACED

# load data
nc_path <- "~/Documents/GitHub/exploration/breath-rate/data/Bm181021-TDR11_prh8.nc"
prh_nc <- ncdf4::nc_open(nc_path)

# read variables
dn <- ncdf4::ncvar_get(prh_nc, "DN")
dt <- breath::dn_to_posix(dn, "US/Pacific")
secs <- as.numeric(dt - dt[1], unit = "secs")
p <- ncdf4::ncvar_get(prh_nc, "P")
pitch <- ncdf4::ncvar_get(prh_nc, "pitch")
speed <- ncdf4::ncvar_get(prh_nc, "speedJJ")
fs <- ncdf4::ncatt_get(prh_nc, "P", "sampling_rate")$value

# Create features (smooth over 2s and calculate rate of change)
feat <- dplyr::tibble(dt, secs, p, pitch, speed) %>%
  dplyr::mutate(p2 = RcppRoll::roll_mean(p, n = fs * 2, fill = NA),
                pitch2 = RcppRoll::roll_mean(pitch, n = fs * 2, fill = NA),
                speed2 = RcppRoll::roll_mean(speed, n = fs * 2, fill = NA)) %>%
  # One week
  dplyr::slice(1:4838400)
attr(feat, "fs") <- fs

usethis::use_data(feat, overwrite = TRUE)

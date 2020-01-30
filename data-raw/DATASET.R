## code to prepare `DATASET` dataset goes here

# Read data, smooth depth, decimate to 1 hz
# Data courtesy James Fahlbusch
bm181021_1hz <- read_deployment(file.choose(), "US/Pacific") %>%
  smooth_p(1) %>%
  decimate(1)
# Truncate at Nov 22 11:30 (tag off)
bm181021_1hz$data <- bm181021_1hz$data %>%
  filter(dt < lubridate::ymd_hm("20181122 1130"))

# Find breaths and dives
bm181021_dives <- find_breaths_dives(
  bm181021_1hz,
  interval =  10,
  surface = 2,
  ibi_thr = 300
)

usethis::use_data(bm181021_dives, overwrite = TRUE)

skip("test manually")

# Read data, smooth depth, decimate to 1 hz
bm181021_1hz <- read_depth(file.choose(), "US/Pacific") %>%
  smooth_p(1) %>%
  decimate(1)

# Find breaths and dives
bm181021_dives <- find_breaths_dives(
  bm181021_1hz,
  interval =  10,
  surface = 2,
  ibi_thr = 300
)

# Plot a dive
plot_dive_event(bm181021_dives, 7, 20)

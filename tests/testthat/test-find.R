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

onedive <- dplyr::filter(bm181021_dives$data, diveid %in% c(1, -1))
breaths_onedive <- dplyr::filter(onedive, is_breath)
ggplot2::ggplot(onedive, ggplot2::aes(dt, p)) +
  ggplot2::geom_line(size = 0.5) +
  ggplot2::geom_point(data = breaths_onedive,
             color = "red") +
  ggplot2::scale_y_reverse() +
  ggplot2::theme_minimal()

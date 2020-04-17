#' Robust peak finder
#'
#' @param x [vector]
#' @param width [vector(1)] width of search window
#' @param prominence [vector(1)] minimum prominence of peaks
#' @param idx_off [vector(1)] internal use only
#'
#' @return [matrix(n, 2)] first column is peak index, second column is peak
#'   prominence. Ordered by prominence.
#' @export
#'
#' @examples
#' set.seed(1202)
#' x <- seq(0, 4 * pi, length.out = 1e2)
#' y <- sin(x^1.4) + rnorm(length(x), sd = 0.25)
#' peaks <- find_peaks(y, width = 10, prominence = 0.5)
#' plot(seq_along(x), y, type = "l")
#' abline(v = peaks[,1], lty = 3)
find_peaks <- function(x, width, prominence, idx_off = 0) {
  if (length(x) < width)
    return(NULL)
  peak_idx <- which.max(x)

  # Skip edges
  if (peak_idx %in% c(1, length(x)))
    return(NULL)

  peak_val <- x[peak_idx]
  left_bound <- last(which(x[1:(peak_idx - 1)] > peak_val))
  if (is.na(left_bound))
    left_bound <- 1
  right_bound <- peak_idx + first(which(x[(peak_idx + 1):length(x)] > peak_val))
  if (is.na(right_bound))
    right_bound <- length(x)
  left_low <- min(x[left_bound:(peak_idx - 1)])
  right_low <- min(x[(peak_idx + 1):right_bound])
  low_val <- max(left_low, right_low)
  peak <- c(peak_idx + idx_off, peak_val - low_val)

  left <- right <- NULL

  # recurse on the left
  new_left <- peak_idx - floor(width / 2)
  if (new_left >= width)
    left <- find_peaks(x[1:new_left],
                       width, prominence,
                       idx_off + 0)

  # recurse on the right
  new_right <- (peak_idx + floor(width / 2))
  if (length(x) - new_right + 1 >= width)
    right <- find_peaks(x[new_right:length(x)],
                        width, prominence,
                        idx_off + new_right - 1)

  if (peak[2] >= prominence)
    rbind(peak, left, right)
  else
    rbind(left, right)
}


#' Breath classifier
#'
#' @param prh [data.frame] with column p2 (smoothed depth)
#' @param period minimum interval between breaths
#' @param prominence minimum depth prominence of breath
#'
#' @return [data.frame] prh filtered to breaths with surface_idx and row_idx
#' @export
#'
#' @examples
#' breaths <- find_breaths(feat, period = 5, prominence = 0.5, surf_thr = 0.5)
find_breaths <- function(prh, period, prominence, surf_thr) {
  fs <- attr(prh, "fs")
  search_surface <- function(data, key) {
    data$is_breath <- FALSE

    if (is.na(key$surface_idx))
      return(data)

    # find local depth minima in 2 x period second window at surface
    peaks <- find_peaks(-data$p2, width = 2 * period * fs, prominence = 0.5)
    if (!is.null(peaks)) {
      peaks_p <- data$p2[peaks[, 1]]
      breath_idx <- peaks[, 1][peaks_p <= surf_thr]
      data$is_breath[breath_idx] <- TRUE
    }

    data
  }

  # find surface intervals
  # find regions of depth<10m for >2s
  depth_rle <- rle(prh$p2 < 10)
  depth_rle$surface <- depth_rle$lengths > 2 * fs & depth_rle$values == TRUE
  depth_rle$start <- cumsum(lag(depth_rle$lengths, default = 1))
  surfaces <- tibble(
    lengths = depth_rle$lengths,
    values = depth_rle$values,
    is_surface = lengths > 10 * fs & values == TRUE,
    surface_idx = ifelse(is_surface, cumsum(is_surface), NA),
    surface_start = cumsum(lag(lengths, default = 1)),
    surface_end = lead(surface_start, default = nrow(feat) + 1) - 1
  ) %>%
    filter(is_surface) %>%
    select(surface_idx:surface_end) %>%
    purrr::pmap_dfr(function(surface_idx, surface_start, surface_end) {
      tibble(surface_idx, secs = prh$secs[surface_start:surface_end])
    })

  # Identify surface intervals, find breaths at surface
  prh %>%
    left_join(surfaces, by = "secs") %>%
    group_by(surface_idx) %>%
    group_modify(search_surface) %>%
    arrange(secs)
}

#' Plot breaths in a surface interval
#'
#' @param prh [data.frame] prh with breaths identified
#' @param surface_idx [integer(1)]
#'
#' @return [grob]
#' @export
#'
#' @examples
#' breaths <- find_breaths(feat, period = 5, prominence = 0.5, surf_thr = 0.5)
#' plot_surface(breaths, 1)
plot_surface <- function(prh, surface_idx) {
  data <- filter(prh, surface_idx == !!surface_idx)
  b <- filter(data, is_breath)
  suppressWarnings(
    cowplot::plot_grid(
      cowplot::ggdraw() +
        cowplot::draw_label(glue::glue("Surface {surface_idx}"),
                            fontface = 'bold',
                            x = 0,
                            hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 7)),
      ggplot(data, aes(dt, p2)) +
        geom_line(size = 0.2, color = "blue") +
        geom_vline(aes(xintercept = dt), b, linetype = 3) +
        scale_x_datetime(date_labels = "%b-%d %H:%M") +
        scale_y_reverse("Depth (m)") +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank()),
      ggplot(data, aes(dt, pitch2 * 180 / pi)) +
        geom_line(size = 0.2, color = "red") +
        geom_vline(aes(xintercept = dt), b, linetype = 3) +
        scale_x_datetime(date_labels = "%b-%d %H:%M") +
        scale_y_continuous("Pitch (deg)", limits = c(-15, 15)) +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank()),
      ggplot(data, aes(dt, speed2)) +
        geom_line(size = 0.2, color = "purple") +
        geom_vline(aes(xintercept = dt), b, linetype = 3) +
        scale_x_datetime(date_labels = "%b-%d %H:%M") +
        scale_y_continuous("Jiggle speed (m/s)") +
        theme_minimal() +
        theme(axis.title.x = element_blank()),
      ncol = 1,
      align = "v",
      rel_heights = c(1, rep(5, 3))
    )
  )
}

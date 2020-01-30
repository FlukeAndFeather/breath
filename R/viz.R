#' Plot a dive event (dive + post-dive interval)
#'
#' @param x dive data with dives/breaths identified (see [find_breaths_dives()])
#' @param diveid dive to plot
#' @param buffer time around dive to plot (in seconds)
#'
#' @return a ggplot showing depth over time with breaths indicated in red
#' @export
plot_dive_event <- function(x, diveid, buffer) {
  # Filter dive and post-dive interval
  dive_event <- c(diveid, -diveid)
  onedive <- dplyr::filter(x$data, diveid %in% dive_event)
  breaths <- dplyr::filter(onedive, is_breath)

  # Expand limits by buffer
  expand <- c(-lubridate::seconds(buffer), lubridate::seconds(buffer))
  lims <- range(onedive$dt) + expand
  data <- dplyr::filter(x$data, dplyr::between(dt, lims[1], lims[2]))

  # Create lunge-depth table
  lunges <- dplyr::tibble(dt = x$lunge_dt) %>%
    dplyr::filter(dt > dplyr::first(onedive$dt),
                  dt < dplyr::last(onedive$dt)) %>%
    mutate(p = stats::approx(x$data$dt,
                             x$data$p,
                             dt)$y)

  # Create plot
  dive_dur <- function(t) {
    as.numeric(dplyr::last(t) - dplyr::first(t), unit = "mins")
  }
  dur <- onedive %>%
    dplyr::filter(diveid == {{ diveid }}) %>%
    dplyr::summarize(dur = dive_dur(dt)) %>%
    dplyr::pull(dur)
  title <- sprintf("Dive %d: %.1f min, %d breaths, %d lunges",
                   diveid,
                   dur,
                   nrow(breaths),
                   nrow(lunges))
  shading <- onedive %>%
    dplyr::filter(diveid == {{ diveid }}) %>%
    dplyr::summarize(
      dt_min = dplyr::first(dt),
      dt_max = dplyr::last(dt)
    )
  ggplot2::ggplot(data, ggplot2::aes(dt, p)) +
    ggplot2::annotate("rect",
                      xmin = shading$dt_min,
                      xmax = shading$dt_max,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = "black",
                      alpha = 0.2) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::geom_point(data = lunges,
                        color = "red") +
    ggplot2::geom_point(data = breaths,
                        color = "blue") +
    ggplot2::scale_x_datetime("", date_labels = "%b %d %H:%M") +
    ggplot2::scale_y_reverse("Depth (m)") +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal()
}

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
  onedive <- filter(x$data, diveid %in% dive_event)
  breaths <- filter(onedive, is_breath)

  # Expand limits by buffer
  expand <- c(-lubridate::seconds(buffer), lubridate::seconds(buffer))
  lims <- range(onedive$dt) + expand
  data <- filter(x$data, between(dt, lims[1], lims[2]))

  # Create lunge-depth table
  if (is.null(x$lunge_dt))
    x$lunge_dt <- numeric(0)
  lunges <- tibble(dt = x$lunge_dt) %>%
    filter(dt > first(onedive$dt),
           dt < last(onedive$dt)) %>%
    mutate(p = stats::approx(x$data$dt,
                             x$data$p,
                             dt)$y)

  # Create plot
  dive_dur <- function(t) {
    as.numeric(last(t) - first(t), unit = "mins")
  }
  dur <- onedive %>%
    filter(diveid == {{ diveid }}) %>%
    summarize(dur = dive_dur(dt)) %>%
    pull(dur)
  title <- sprintf("Dive %d: %.1f min, %d breaths, %d lunges",
                   diveid,
                   dur,
                   nrow(breaths),
                   nrow(lunges))
  shading <- onedive %>%
    filter(diveid == {{ diveid }}) %>%
    summarize(
      dt_min = first(dt),
      dt_max = last(dt)
    )
  ggplot(data, aes(dt, p)) +
    annotate("rect",
                      xmin = shading$dt_min,
                      xmax = shading$dt_max,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = "black",
                      alpha = 0.2) +
    geom_line(size = 0.5) +
    geom_point(data = lunges,
                        color = "red") +
    geom_point(data = breaths,
                        color = "blue") +
    scale_x_datetime("", date_labels = "%b %d %H:%M") +
    scale_y_reverse("Depth (m)") +
    labs(title = title) +
    theme_minimal()
}

#' Plot an aerobic dive limit model
#'
#' After fitting an aerobic dive limit (ADL) model using [fit_adl()], [plot_adl()] visualizes the ADL, the quantile regressions, and their intersection.
#'
#' @param fitted_adl an ADL model (see [fit_adl()])
#' @param ylim number of breaths to limit the y-axes (0-25 by default)
#'
#' @return a ggplot object with post-dive breath counts plotted against dive duration superimposed with the ADL and the intersecting quantile regression lines.
#' @export
#'
#' @examples
#'
#' fit_adl(bm181021_dives) %>%
#'   plot_adl()
plot_adl <- function(fitted_adl, ylim = c(0, 25)) {
  with(fitted_adl, {
    intersection <- solve(
      matrix(c(1, 1, -coef(rq_left)[2], -coef(rq_right)[2]), ncol = 2),
      matrix(c(coef(rq_left)[1], coef(rq_right)[1]), ncol = 1)
    )
    rq_segs <- tibble(
      side = c("left", "right"),
      intercept = c(coef(rq_left)[1], coef(rq_right)[1]),
      slope = c(coef(rq_left)[2], coef(rq_right)[2]),
      xmin = c(min(dives$duration), intersection[2]),
      xmax = c(intersection[2], max(dives$duration)),
      ymin = intercept + slope * xmin,
      ymax = intercept + slope * xmax
    )
    ggplot(dives, aes(duration, n_breaths)) +
      geom_point(color = "gray", shape = 1) +
      geom_vline(xintercept = adl,
                          linetype = "dashed",
                          size = 1.5) +
      geom_segment(aes(x = xmin, xend = xmax,
                                         y = ymin, yend = ymax,
                                         color = side),
                            data = rq_segs,
                            size = 1.5) +
      annotate(
        "text",
        x = adl * 1.05,
        y = ylim[1] + 0.8 * diff(ylim),
        label = sprintf("ADL = %.1f min\n(over %d dives)", adl / 60, n_dives),
        hjust = 0
      ) +
      scale_x_continuous(
        "Duration (min)",
        breaks = function(lim) 60 * pretty(lim / 60),
        labels = scales::label_number(accuracy = 1, scale = 1 / 60)
      ) +
      scale_y_continuous("Count of breaths",
                                  limits = ylim) +
      scale_color_manual(values = c("orange", "purple")) +
      expand_limits(x = 0, y = 0) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

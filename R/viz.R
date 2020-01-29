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
  onedive <- dplyr::filter(x$data, diveid %in% c({{ diveid }}, - {{ diveid }}))
  breaths_onedive <- dplyr::filter(onedive, is_breath)

  # Expand limits by buffer
  expand <- c(-lubridate::seconds(buffer), lubridate::seconds(buffer))
  lims <- range(onedive$dt) + expand
  data <- dplyr::filter(x$data, dplyr::between(dt, lims[1], lims[2]))

  # Create plot
  title <- sprintf("Dive %d, %d breaths", diveid, nrow(breaths_onedive))
  ggplot2::ggplot(data, ggplot2::aes(dt, p)) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::geom_point(data = breaths_onedive,
                        color = "red") +
    ggplot2::scale_x_datetime("", date_labels = "%b %d %H:%M") +
    ggplot2::scale_y_reverse("Depth (m)") +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal()
}

#' Summarize dives in a deployment
#'
#' @param x depth/time object with breaths and dives identified
#'
#' @return a tibble summarizing each dive:
#'     \itemize{
#'       \item{duration}
#'       \item{depth}
#'       \item{n_breaths}
#'     }
#' @export
summarize_dives <- function(x) {
  fs <- x$fs
  x$data %>%
    # Remove invalid dives
    filter(diveid != 0) %>%
    mutate(dive_event = abs(diveid)) %>%
    group_by(dive_event) %>%
    summarize(
      duration = fs * sum(diveid > 0),
      depth = max(p),
      n_breaths = sum(is_breath),
      n_lunges = count_lunges(x, dt)
    )
}

summarize_time <- function(x) {
  x$data %>%
    mutate(hour = floor(as.numeric(dt - dt[1], unit = "hours"))) %>%
    group_by(hour) %>%
    summarize(
      hour_start = dt[1],
      n_breaths = sum(is_breath),
      n_lunges = count_lunges(x, dt)
    )
}

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
    dplyr::filter(diveid != 0) %>%
    dplyr::mutate(dive_event = abs(diveid)) %>%
    dplyr::group_by(dive_event) %>%
    dplyr::summarize(
      duration = fs * sum(diveid > 0),
      depth = max(p),
      n_breaths = sum(is_breath)
    )
}

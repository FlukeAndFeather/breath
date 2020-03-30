#' Find breaths and dives in depth data
#'
#' @param x a depth/time object (see [read_deployment()])
#' @param interval typical interval between breaths in seconds (e.g. 10 for blue whales)
#' @param surface minimum depth for a breath
#' @param ibi_thr inter-breath interval threshold for a dive
#'
#' @return a depth/time object with identified breaths and dives
#' @export
find_breaths_dives <- function(x, interval, surface, ibi_thr) {
  # Find breaths
  breaths <- x$data %>%
    filter(
      p == RcppRoll::roll_min(p, interval * 2, fill = NA),
      p < surface
    ) %>%
    mutate(ibi = as.numeric(lead(dt) - dt, unit = "secs")) %>%
    filter(ibi > interval) %>%
    select(-p)

  # Label breaths and dives
  x$data <- x$data %>%
    left_join(breaths, by = "dt") %>%
    tidyr::replace_na(list(ibi = 0)) %>%
    mutate(
      is_breath = ibi > 0,
      last_ibi = stats::approx(breaths$dt,
                               breaths$ibi,
                               dt,
                               "constant",
                               yleft = 0)$y,
      diveid0 = lag(cumsum(ibi >= ibi_thr), default = 0),
      diveid = case_when(
        last_ibi >= ibi_thr & !is_breath ~ diveid0,
        last_ibi <  ibi_thr & !is_breath ~ -diveid0,
        is_breath                        ~ -diveid0,
        TRUE                             ~ 0
      ),
      ibi = ifelse(ibi == 0, NA_real_, ibi)
    ) %>%
    select(dt, p, diveid, is_breath, ibi)

  x
}

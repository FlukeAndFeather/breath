#' Calculate the combined root mean square error
#'
#' [combined_rmse()] segments dive data by duration, fits two quantile
#' regressions, and returns the combined root mean square error (RMSE) of both
#' regressions.
#'
#' @param breakpoint duration to segment the dive data
#' @param data dive data with breaths and dives identified (see
#'   [find_breaths_dives()])
#' @param tau which quantile to regress (0.05 by default)
#'
#' @return the combined RMSE of the segmented regression
combined_rmse <- function(breakpoint, data, tau = 0.05) {
  data1 <- dplyr::filter(data, duration < breakpoint)
  data2 <- dplyr::filter(data, duration >= breakpoint)
  rq1 <- quantreg::rq(n_breaths ~ duration, tau = tau, data = data1)
  rq2 <- quantreg::rq(n_breaths ~ duration, tau = tau, data = data2)
  sqrt(sum(resid(rq1)^2, resid(rq2)^2) / nrow(data))
}

#' Fit a behavioral aerobic dive limit model to dive data
#'
#' Roughly following the methods of Horning (2012), [fit_adl()] searches for a duration beyond which the post-dive recovery increases.
#'
#' @param x dive data with dives/breaths identified (see [find_breaths_dives()])
#' @param n_dives number of dives to average durations and breaths over
#' @param nofeeding if true, removes dives with lunges
#'
#' @return a list with the aerobic dive limit (ADL), quantile regressions left and right of the ADL, whether it passes the continuity requirement, the original dive data, and the n_dives and nofeeding parameter values.
#' @export
#'
#' @importFrom stats coef quantile resid
#'
#' @examples
#'
#' fit_adl(bm181021_dives) %>%
#'   plot_adl()
fit_adl <- function(x, n_dives = 1, nofeeding = TRUE) {
  # Aggregate dives, possibly removing feeding
  dives <- summarize_dives(x) %>%
    dplyr::mutate_at(
      dplyr::vars(duration, n_breaths, n_lunges),
      ~ RcppRoll::roll_suml(.x, n = n_dives, fill = NA) / n_dives
    ) %>%
    dplyr::filter((n_lunges == 0) | !nofeeding)

  # Find breakpoint that minimizes combined RMSE (between median and 90th
  # percentile)
  breaks <- seq(quantile(dives$duration, 0.5),
                quantile(dives$duration, 0.9),
                length.out = 5e2)
  rmse <- purrr::map_dbl(breaks, combined_rmse, data = dives)
  adl <- breaks[which.min(rmse)]

  # Do lines cross near ADL? I.e. continuity restriction
  rq_left <- quantreg::rq(n_breaths ~ duration,
                          tau = 0.05,
                          data = dives,
                          subset = dives$duration < adl)
  rq_right <- quantreg::rq(n_breaths ~ duration,
                           tau = 0.05,
                           data = dives,
                           subset = dives$duration >= adl)
  intersection <- solve(
    matrix(c(1, 1, -coef(rq_left)[2], -coef(rq_right)[2]), ncol = 2),
    matrix(c(coef(rq_left)[1], coef(rq_right)[1]), ncol = 1)
  )
  continuity <- intersection[2] >= 0.9 * adl && intersection[2] <= 1.1 * adl

  # Return the ADL, the two quantile regressions, and the continuity results
  # Include the dive summary and parameters (n_dives, nofeeding)
  list(adl = adl,
       rq_left = rq_left,
       rq_right = rq_right,
       continuity = continuity,
       dives = dives,
       n_dives = n_dives,
       nofeeding = nofeeding)
}


# Adapted from Matlab's signal::find_peaks
# https://www.mathworks.com/help/signal/ref/findpeaks.html#buff2uu

# Prominence
#
# The prominence of a peak measures how much the peak stands out due to its intrinsic height and its location relative to other peaks. A low isolated peak can be more prominent than one that is higher but is an otherwise unremarkable member of a tall range.
#
# To measure the prominence of a peak:
#
#   1) Place a marker on the peak.
#
# Extend a horizontal line from the peak to the left and right until the line does one of the following:
#
#   Crosses the signal because there is a higher peak
#
# Reaches the left or right end of the signal
#
# Find the minimum of the signal in each of the two intervals defined in Step 2. This point is either a valley or one of the signal endpoints.
#
# The higher of the two interval minima specifies the reference level. The height of the peak above this level is its prominence.
#
# findpeaks makes no assumption about the behavior of the signal beyond its endpoints, whatever their height. This is reflected in Steps 2 and 4 and often affects the value of the reference level. Consider for example the peaks of this signal:

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



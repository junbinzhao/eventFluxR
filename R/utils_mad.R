# Robust MAD-based hot-moment flag (used internally by identify_hot_moments())
.mad_flag <- function(x, k = 3,
                      center = c("median", "mean"),
                      scale = c("robust_sd", "mad"),
                      constant = 1.4826,
                      na.rm = TRUE,
                      one_sided = TRUE) {
  center <- match.arg(center)
  scale  <- match.arg(scale)

  ctr <- if (center == "median") stats::median(x, na.rm = na.rm) else mean(x, na.rm = na.rm)
  mad_raw <- stats::mad(x, constant = 1, na.rm = na.rm)
  sigma <- if (scale == "robust_sd") constant * mad_raw else mad_raw

  if (!is.finite(sigma) || sigma <= 0) {
    return(rep(FALSE, length(x)))
  }

  if (one_sided) {
    (x - ctr) > k * sigma
  } else {
    abs(x - ctr) > k * sigma
  }
}

# Flagged-count sensitivity of .mad_flag() across candidate k thresholds
.mad_threshold_sensitivity <- function(x, k_values, ...) {
  out <- lapply(k_values, function(k) {
    flag <- .mad_flag(x, k = k, ...)
    data.frame(k = k,
               n_flagged = sum(flag, na.rm = TRUE),
               pct_flagged = 100 * mean(flag, na.rm = TRUE))
  })
  do.call(rbind, out)
}

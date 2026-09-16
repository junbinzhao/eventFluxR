#' Compute a decay-weighted fertilization index
#'
#' For each timestamp, sums the exponentially decayed contribution of every
#' past fertilization event (`dose * exp(-k * days_since)`), and reports the
#' days elapsed since the most recent event. Generalizes the "fert_ef" /
#' "D_fertiliz" variables used in the Svanhovd N2O flux models.
#'
#' @param timestamps Vector of times (`Date`/`POSIXct`/numeric) at which to
#'   evaluate the index.
#' @param event_times Vector of fertilization event times (same class
#'   family as `timestamps`).
#' @param event_doses Numeric vector of doses applied at `event_times` (e.g.
#'   kg N ha-1); recycled if length 1.
#' @param k Decay rate constant per unit of `units` (default 0.05/day;
#'   half-life = `log(2)/k`). Ignored if `half_life` is supplied.
#' @param half_life Optional half-life (same unit as `units`); if supplied,
#'   overrides `k` via `k = log(2) / half_life`.
#' @param units Units passed to `difftime()` (default `"days"`).
#'
#' @return A data frame with one row per `timestamps` value, containing
#'   `fert_index` (the decayed, summed dose) and `days_since_fertiliz`
#'   (days since the closest prior event, `NA` if none had occurred yet).
#' @export
#'
#' @examples
#' data(n2o_sample)
#'
#' # two fertilization events during the 2022 season at Svanhovd (High plots)
#' events <- as.POSIXct(c("2022-05-02", "2022-08-09"), tz = "UTC")
#' idx <- fertilization_index(
#'   n2o_sample$Timestamp, events,
#'   event_doses = c(90, 36), half_life = 14
#' )
#' head(idx)
fertilization_index <- function(timestamps, event_times, event_doses,
                                 k = 0.05, half_life = NULL, units = "days") {
  if (!is.null(half_life)) k <- log(2) / half_life
  event_doses <- rep_len(event_doses, length(event_times))

  fert_index <- vapply(timestamps, function(t) {
    days_since <- as.numeric(difftime(t, event_times, units = units))
    valid <- days_since >= 0
    if (!any(valid)) return(0)
    sum(event_doses[valid] * exp(-k * days_since[valid]))
  }, numeric(1))

  days_since_fertiliz <- vapply(timestamps, function(t) {
    d <- as.numeric(difftime(t, event_times, units = units))
    d <- d[d >= 0]
    if (length(d) == 0) return(NA_real_)
    min(d)
  }, numeric(1))

  data.frame(fert_index = fert_index, days_since_fertiliz = days_since_fertiliz)
}

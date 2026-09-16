# Collapse consecutive flagged rows (within each group) into events with a
# start/end time and duration (used internally by identify_hot_moments())
.event_duration <- function(data, time_col, flag_col, group_vars,
                            min_duration_hours = 1) {
  data$.flag <- as.integer(as.logical(data[[flag_col]]))
  data$.grp <- interaction(data[group_vars], drop = TRUE)
  data <- data[order(data$.grp, data[[time_col]]), ]

  events <- lapply(split(data, data$.grp), function(df) {
    if (nrow(df) == 0) return(NULL)
    prev <- c(0L, utils::head(df$.flag, -1))
    new_event <- df$.flag == 1L & prev == 0L
    event_id <- cumsum(new_event) * df$.flag
    df$.event_id <- event_id

    hot <- df[df$.flag == 1L, , drop = FALSE]
    if (nrow(hot) == 0) return(NULL)

    do.call(rbind, lapply(split(hot, hot$.event_id), function(ev) {
      out <- data.frame(
        event_id = ev$.event_id[1],
        start_time = min(ev[[time_col]]),
        end_time = max(ev[[time_col]]),
        n_points = nrow(ev)
      )
      out$duration_hours <- as.numeric(difftime(out$end_time, out$start_time, units = "hours"))
      out$duration_hours <- ifelse(out$duration_hours == 0, min_duration_hours, out$duration_hours)
      out$duration_days <- out$duration_hours / 24
      for (gv in group_vars) out[[gv]] <- ev[[gv]][1]
      out
    }))
  })

  out <- do.call(rbind, events)
  rownames(out) <- NULL
  out[, c(group_vars, "event_id", "start_time", "end_time",
          "duration_hours", "duration_days", "n_points")]
}

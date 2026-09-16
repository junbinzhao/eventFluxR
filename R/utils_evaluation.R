# Detection rate / false alarm rate across a threshold grid
.detection_metrics <- function(y_true, y_prob, thresholds) {
  y_true <- as.integer(as.logical(y_true))
  out <- lapply(thresholds, function(thr) {
    y_pred <- as.integer(y_prob >= thr)
    TP <- sum(y_pred == 1 & y_true == 1)
    FP <- sum(y_pred == 1 & y_true == 0)
    FN <- sum(y_pred == 0 & y_true == 1)
    detection_rate <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
    false_alarm_rate <- if (sum(y_true == 0) > 0) FP / sum(y_true == 0) else NA_real_
    data.frame(threshold = thr,
               detection_rate = detection_rate,
               false_alarm_rate = false_alarm_rate,
               youden_J = detection_rate - false_alarm_rate)
  })
  do.call(rbind, out)
}

# Precision-recall AUC via trapezoidal integration (no extra dependency)
.pr_auc <- function(y_true, y_prob) {
  y_true <- as.integer(as.logical(y_true))
  ord <- order(y_prob, decreasing = TRUE)
  y_sorted <- y_true[ord]

  tp <- cumsum(y_sorted == 1)
  fp <- cumsum(y_sorted == 0)
  n_pos <- sum(y_true == 1)

  precision <- tp / (tp + fp)
  recall <- tp / n_pos

  # anchor the curve at (recall = 0, precision = 1) before integrating
  recall <- c(0, recall)
  precision <- c(1, precision)

  sum(diff(recall) * (utils::head(precision, -1) + utils::tail(precision, -1)) / 2)
}

# RMSE / MAE / mean error, optionally split by a grouping factor
.regression_metrics <- function(obs, pred, by = NULL) {
  one_group <- function(o, p, label) {
    data.frame(group = label, n = length(o),
               RMSE = sqrt(mean((o - p)^2, na.rm = TRUE)),
               MAE = mean(abs(o - p), na.rm = TRUE),
               ME = mean(p - o, na.rm = TRUE))
  }

  if (is.null(by)) {
    return(one_group(obs, pred, "All data"))
  }

  out <- do.call(rbind, lapply(split(seq_along(obs), by), function(idx) {
    one_group(obs[idx], pred[idx], as.character(by[idx][1]))
  }))
  rbind(out, one_group(obs, pred, "All data"))
}

# Background vs. hot-moment magnitude tiers (quantile-based cut points)
.magnitude_bins <- function(y, is_hot, probs = c(0.95, 0.99),
                           labels = c("Low HM", "Mid HM", "High HM")) {
  is_hot <- as.logical(is_hot)
  q <- stats::quantile(y, probs = probs, na.rm = TRUE)

  bin <- rep("Background", length(y))
  hot_idx <- which(is_hot)
  cuts <- c(-Inf, q, Inf)
  hot_labels <- labels[findInterval(y[hot_idx], cuts, all.inside = TRUE)]
  bin[hot_idx] <- hot_labels

  factor(bin, levels = c("Background", labels))
}

# Bias diagnostics restricted to the upper tail of the observed distribution
.upper_tail_bias <- function(obs, pred, q_prob = 0.95) {
  thr <- stats::quantile(obs, probs = q_prob, na.rm = TRUE)
  idx <- which(obs >= thr)
  o <- obs[idx]
  p <- pred[idx]

  data.frame(
    threshold = as.numeric(thr),
    n = length(idx),
    ME = mean(p - o, na.rm = TRUE),
    MAE = mean(abs(p - o), na.rm = TRUE),
    RMSE = sqrt(mean((p - o)^2, na.rm = TRUE)),
    relative_bias_pct = 100 * mean(p - o, na.rm = TRUE) / mean(o, na.rm = TRUE)
  )
}

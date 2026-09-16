#' Evaluate hot-moment classification and/or flux regression performance
#'
#' Computes classification diagnostics (detection rate / false alarm rate
#' across thresholds, PR-AUC) when `y_true`/`y_prob` are supplied, and
#' regression diagnostics (RMSE, MAE, ME overall and by flux-magnitude
#' class, plus upper-tail bias) when `obs`/`pred` are supplied. Either group
#' of arguments (or both) may be provided.
#'
#' @param obs,pred Observed and predicted flux values (regression
#'   evaluation).
#' @param y_true,y_prob Binary ground-truth labels and predicted
#'   probabilities (classification evaluation).
#' @param thresholds Threshold grid for the detection-rate/false-alarm-rate
#'   table.
#' @param mag_probs,mag_labels Quantile probabilities and labels used to
#'   split hot moments into magnitude tiers for the by-class regression
#'   metrics; ignored if `y_true` is `NULL`.
#' @param upper_tail_q Quantile probability defining the upper tail for the
#'   bias diagnostic (default 0.95).
#'
#' @return A list of class `"flux_model_eval"` with any of `detection`
#'   (data frame), `pr_auc` (numeric), `regression` (data frame, by class if
#'   `y_true` supplied), and `upper_tail_bias` (data frame), depending on
#'   which inputs were supplied.
#' @export
#'
#' @examples
#' data(n2o_sample)
#' set.seed(1)
#' # toy predictions/probabilities for illustration
#' pred <- n2o_sample$N2O_flux + rnorm(nrow(n2o_sample), 0, 0.3)
#' prob <- stats::plogis(scale(pred)[, 1])
#'
#' ev <- evaluate_flux_model(
#'   obs = n2o_sample$N2O_flux, pred = pred,
#'   y_true = n2o_sample$y_bin, y_prob = prob
#' )
#' ev$regression
#' ev$detection
#' ev$upper_tail_bias
evaluate_flux_model <- function(obs = NULL, pred = NULL,
                                 y_true = NULL, y_prob = NULL,
                                 thresholds = seq(0.1, 0.9, by = 0.05),
                                 mag_probs = c(0.95, 0.99),
                                 mag_labels = c("Low HM", "Mid HM", "High HM"),
                                 upper_tail_q = 0.95) {
  out <- list()

  if (!is.null(y_true) && !is.null(y_prob)) {
    out$detection <- .detection_metrics(y_true, y_prob, thresholds = thresholds)
    out$pr_auc <- .pr_auc(y_true, y_prob)
  }

  if (!is.null(obs) && !is.null(pred)) {
    by <- NULL
    if (!is.null(y_true)) {
      by <- .magnitude_bins(obs, as.logical(y_true), probs = mag_probs, labels = mag_labels)
    }
    out$regression <- .regression_metrics(obs, pred, by = by)
    out$upper_tail_bias <- .upper_tail_bias(obs, pred, q_prob = upper_tail_q)
  }

  class(out) <- "flux_model_eval"
  out
}

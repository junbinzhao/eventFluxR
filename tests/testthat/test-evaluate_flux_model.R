test_that("evaluate_flux_model computes classification metrics when y_true/y_prob given", {
  y <- c(1, 1, 0, 0)
  p <- c(0.9, 0.4, 0.4, 0.1)
  ev <- evaluate_flux_model(y_true = y, y_prob = p, thresholds = 0.5)
  expect_s3_class(ev, "flux_model_eval")
  expect_equal(ev$detection$detection_rate, 0.5)
  expect_equal(ev$detection$false_alarm_rate, 0)
  expect_true(is.numeric(ev$pr_auc))
  expect_null(ev$regression)
})

test_that("evaluate_flux_model computes regression metrics, by class when y_true given", {
  obs <- c(1, 2, 20, 30)
  pred <- c(1, 2, 18, 28)
  y_true <- c(0, 0, 1, 1)
  ev <- evaluate_flux_model(obs = obs, pred = pred, y_true = y_true,
                            mag_probs = c(0.5), mag_labels = c("Low HM", "High HM"))
  expect_true("All data" %in% ev$regression$group)
  expect_true(nrow(ev$regression) > 1)
  expect_equal(ev$upper_tail_bias$n > 0, TRUE)
  expect_null(ev$detection)
})

test_that("evaluate_flux_model returns both blocks when all inputs supplied", {
  obs <- c(1, 1, 1, 10)
  pred <- c(1, 1, 1, 8)
  y_true <- c(0, 0, 0, 1)
  y_prob <- c(0.1, 0.2, 0.3, 0.9)
  ev <- evaluate_flux_model(obs, pred, y_true, y_prob)
  expect_true(all(c("detection", "pr_auc", "regression", "upper_tail_bias") %in% names(ev)))
})

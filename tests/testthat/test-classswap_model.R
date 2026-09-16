test_that("classswap_model fits and predicts a numeric vector of the right length", {
  skip_if_not_installed("xgboost")
  set.seed(1)
  n <- 400
  df <- data.frame(
    Chamber = rep(c("A", "B"), each = n / 2),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  df$y_bin <- as.integer(df$x1 + df$x2 > 1)
  df$flux <- ifelse(df$y_bin == 1, stats::rnorm(n, 8, 1), stats::rnorm(n, 1, 0.2))

  fit <- classswap_model(
    df, response_col = "flux", feature_cols = c("x1", "x2"),
    group_vars = "Chamber", label_col = "y_bin",
    n_trials = 2, nrounds = 20, early_stopping_rounds = 5, verbose = FALSE
  )

  expect_s3_class(fit, "classswap_model")
  expect_true(all(c("classifier", "model_bg", "model_hot") %in% names(fit)))

  pred <- predict(fit, df)
  expect_equal(length(pred), nrow(df))
  expect_true(all(is.finite(pred)))
})

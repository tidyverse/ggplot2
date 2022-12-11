test_that("geom_quantile matches quantile regression", {
  skip_if(packageVersion("base") < "3.6.0") # warnPartialMatchArgs didn't accept FALSE
  withr::local_options(
    warnPartialMatchArgs = FALSE,
    warnPartialMatchDollar = FALSE
  )
  skip_if_not_installed("quantreg")

  set.seed(6531)
  x <- rnorm(10)
  df <- tibble::tibble(
    x = x,
    y = x^2 + 0.5 * rnorm(10)
  )

  ps <- ggplot(df, aes(x, y)) + geom_quantile()

  quants <- c(0.25, 0.5, 0.75)

  pred_rq <- predict(
    quantreg::rq(y ~ x,
      tau = quants,
      data = df
    ),
    data_frame(
      x = seq(min(x), max(x), length.out = 100)
    )
  )

  pred_rq <- cbind(seq(min(x), max(x), length.out = 100), pred_rq)
  colnames(pred_rq) <- c("x", paste("Q", quants * 100, sep = "_"))

  # pred_rq is a matrix; convert it to data.frame so that it can be compared
  pred_rq <- as.data.frame(pred_rq)

  ggplot_data <- layer_data(ps)

  pred_rq_test_25 <- pred_rq[, c("x", "Q_25")]
  colnames(pred_rq_test_25) <- c("x", "y")

  # Use expect_equal(ignore_attr = TRUE) to ignore rownames
  expect_equal(
    ggplot_data[ggplot_data$quantile == 0.25, c("x", "y")],
    pred_rq_test_25,
    ignore_attr = TRUE
  )

  pred_rq_test_50 <- pred_rq[, c("x", "Q_50")]
  colnames(pred_rq_test_50) <- c("x", "y")

  expect_equal(
    ggplot_data[ggplot_data$quantile == 0.5, c("x", "y")],
    pred_rq_test_50,
    ignore_attr = TRUE
  )

  pred_rq_test_75 <- pred_rq[, c("x", "Q_75")]
  colnames(pred_rq_test_75) <- c("x", "y")

  expect_equal(
    ggplot_data[ggplot_data$quantile == 0.75, c("x", "y")],
    pred_rq_test_75,
    ignore_attr = TRUE
  )
})

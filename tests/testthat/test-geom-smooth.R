context("geom_smooth")

test_that("data is ordered by x", {
  df <- data.frame(x = c(1, 5, 2, 3, 4), y = 1:5)

  ps <- ggplot(df, aes(x, y))+
    geom_smooth(stat = "identity", se = FALSE)

  expect_equal(layer_data(ps)[c("x", "y")], df[order(df$x), ])
})

test_that("default smoothing methods for small and large data sets work", {
  # test small data set
  set.seed(6531)
  x <- rnorm(10)
  df <- data.frame(
    x = x,
    y = x^2 + 0.5 * rnorm(10)
  )

  m <- loess(y ~ x, data = df, span = 0.75)
  range <- range(df$x, na.rm = TRUE)
  xseq <- seq(range[1], range[2], length.out = 80)
  out <- predict(m, data.frame(x = xseq))
  p <- ggplot(df, aes(x, y)) + geom_smooth()

  expect_message(
    plot_data <- layer_data(p),
    "method = 'loess' and formula 'y ~ x'"
  )
  expect_equal(plot_data$y, as.numeric(out))

  # test large data set
  x <- rnorm(1001) # 1000 is the cutoff point for gam
  df <- data.frame(
    x = x,
    y = x^2 + 0.5 * rnorm(1001)
  )

  m <- mgcv::gam(y ~ s(x, bs = "cs"), data = df)
  range <- range(df$x, na.rm = TRUE)
  xseq <- seq(range[1], range[2], length.out = 80)
  out <- predict(m, data.frame(x = xseq))
  p <- ggplot(df, aes(x, y)) + geom_smooth()

  expect_message(
    plot_data <- layer_data(p),
    "method = 'gam' and formula 'y ~ s\\(x, bs = \"cs\"\\)"
  )
  expect_equal(plot_data$y, as.numeric(out))
})


# Visual tests ------------------------------------------------------------

test_that("geom_smooth() works with alternative stats", {
  df <- data.frame(x = c(1, 1, 2, 2, 1, 1, 2, 2),
                   y = c(1, 2, 2, 3, 2, 3, 1, 2),
                   fill = c(rep("A", 4), rep("B", 4)))

  expect_doppelganger("ribbon turned on in geom_smooth", {
    ggplot(df, aes(x, y, color = fill, fill = fill)) +
      geom_smooth(stat = "summary") # ribbon on by default
  })

  expect_doppelganger("ribbon turned off in geom_smooth", {
    ggplot(df, aes(x, y, color = fill, fill = fill)) +
      geom_smooth(stat = "summary", se = FALSE) # ribbon is turned off via `se = FALSE`
  })
})

test_that("data is ordered by x", {
  df <- data_frame(x = c(1, 5, 2, 3, 4), y = 1:5)

  ps <- ggplot(df, aes(x, y))+
    geom_smooth(stat = "identity", se = FALSE)

  expect_equal(get_layer_data(ps)[c("x", "y")], df[order(df$x), ], ignore_attr = TRUE)
})

test_that("geom_smooth works in both directions", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_smooth(method = 'loess', formula = y ~ x)
  x <- get_layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(mpg, aes(hwy, displ)) +
    geom_smooth(orientation = "y", method = 'loess', formula = y ~ x)
  y <- get_layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])
})

test_that("default smoothing methods for small and large data sets work", {
  skip_if(packageVersion("base") < "3.6.0") # warnPartialMatchArgs didn't accept FALSE
  withr::local_options(warnPartialMatchArgs = FALSE)
  # Numeric differences on the MLK machine on CRAN makes these test fail
  # on that particular machine
  skip_on_cran()

  # test small data set
  set.seed(6531)
  x <- rnorm(10)
  df <- data_frame(
    x = x,
    y = x^2 + 0.5 * rnorm(10)
  )

  m <- loess(y ~ x, data = df, span = 0.75)
  range <- range(df$x, na.rm = TRUE)
  xseq <- seq(range[1], range[2], length.out = 80)
  out <- predict(m, data_frame(x = xseq))
  p <- ggplot(df, aes(x, y)) + geom_smooth()

  expect_message(
    plot_data <- get_layer_data(p),
    "method = 'loess' and formula = 'y ~ x'"
  )
  expect_equal(plot_data$y, as.numeric(out))

  # test large data set
  x <- rnorm(1001) # 1000 is the cutoff point for gam
  df <- data_frame(
    x = x,
    y = x^2 + 0.5 * rnorm(1001)
  )

  skip_if_not_installed("mgcv")

  m <- mgcv::gam(y ~ s(x, bs = "cs"), data = df, method = "REML")
  range <- range(df$x, na.rm = TRUE)
  xseq <- seq(range[1], range[2], length.out = 80)
  out <- predict(m, data_frame(x = xseq))
  p <- ggplot(df, aes(x, y)) + geom_smooth()

  expect_message(
    plot_data <- get_layer_data(p),
    "method = 'gam' and formula = 'y ~ s\\(x, bs = \"cs\"\\)"
  )
  expect_equal(plot_data$y, as.numeric(out))

  # backwards compatibility of method = "auto"
  p <- ggplot(df, aes(x, y)) + geom_smooth(method = "auto")

  expect_message(
    plot_data <- get_layer_data(p),
    "method = 'gam' and formula = 'y ~ s\\(x, bs = \"cs\"\\)"
  )
  expect_equal(plot_data$y, as.numeric(out))
})

test_that("geom_smooth() works when one group fails", {
  # Group A fails, B succeeds
  df <- data_frame0(
    x = c(1, 2, 1, 2, 3),
    y = c(1, 2, 3, 2, 1),
    g = rep(c("A", "B"), 2:3)
  )
  p <- ggplot(df, aes(x, y, group = g)) +
    geom_smooth(method = "loess", formula = y ~ x)

  suppressWarnings(
    expect_warning(ld <- get_layer_data(p), "Failed to fit group 1")
  )
  expect_equal(unique(ld$group), 2)
  expect_gte(nrow(ld), 2)
})

test_that("a fallback message is thrown when `method = 'gam'` and {mgcv} is absent", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_smooth(method = "gam", formula = y ~ x)

  with_mocked_bindings(
    expect_message(
      ggplot_build(p), regexp = "Falling back to `method = \"lm\"`"
    ),
    is_installed = function(...) FALSE
  )
})

# Visual tests ------------------------------------------------------------

test_that("geom_smooth() works with alternative stats", {
  df <- data_frame(x = c(1, 1, 2, 2, 1, 1, 2, 2),
                   y = c(1, 2, 2, 3, 2, 3, 1, 2),
                   fill = c(rep("A", 4), rep("B", 4)))

  expect_doppelganger("ribbon turned on in geom_smooth", {
    ggplot(df, aes(x, y, color = fill, fill = fill)) +
      geom_smooth(stat = "summary", fun.data = mean_se) # ribbon on by default
  })

  expect_doppelganger("ribbon turned off in geom_smooth", {
    ggplot(df, aes(x, y, color = fill, fill = fill)) +
      geom_smooth(stat = "summary", se = FALSE, fun.data = mean_se) # ribbon is turned off via `se = FALSE`
  })
})

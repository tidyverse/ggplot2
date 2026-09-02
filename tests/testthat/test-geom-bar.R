test_that("geom_bar removes bars with parts outside the plot limits", {
  dat <- data_frame(x = c("a", "b", "b", "c", "c", "c"))

  p <- ggplot(dat, aes(x)) + geom_bar()

  # warning created at render stage
  expect_snapshot_warning(ggplotGrob(p + ylim(0, 2.5)))
})

test_that("geom_bar works in both directions", {
  dat <- data_frame(x = c("a", "b", "b", "c", "c", "c"))

  p <- ggplot(dat, aes(x)) + geom_bar()
  x <- get_layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(dat, aes(y = x)) + geom_bar()
  y <- get_layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE))
})

test_that("geom_bar default widths considers panels", {

  dat <- data_frame0(x = c(1:2, 1:2 + 0.1), y = 1,
                    PANEL = factor(rep(1:2, each = 2)))

  layer  <- geom_bar()
  params <- layer$geom_params

  # Default should be panel-wise resolution (0.9), not data-wise resolution (0.1)
  new <- layer$geom$setup_data(dat, params)
  expect_equal(
    new$xmax - new$xmin,
    rep(0.9, 4)
  )

  # Check that default can still be overridden
  params$width <- 0.5
  new <- layer$geom$setup_data(dat, params)
  expect_equal(
    new$xmax - new$xmin,
    rep(0.5, 4)
  )
})

test_that("geom_col removes columns with parts outside the plot limits", {
  dat <- data_frame(x = c(1, 2, 3))

  p <- ggplot(dat, aes(x, x)) + geom_col()

  # warnings created at render stage
  expect_snapshot_warning(ggplotGrob(p + ylim(0.5, 4)))
  expect_snapshot_warning(ggplotGrob(p + ylim(0, 2.5)))
})

test_that("geom_col works in both directions", {
  dat <- data_frame(x = c("a", "b", "c"), y = c(1.2, 2.5, 3.1))

  p <- ggplot(dat, aes(x, y)) + geom_col()
  x <- get_layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(dat, aes(y, x)) + geom_col()
  y <- get_layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])
})

test_that("geom_col supports alignment of columns", {
  dat <- data_frame(x = c("a", "b"), y = c(1.2, 2.5))

  p <- ggplot(dat, aes(x, y)) + geom_col(just = 0.5)
  y <- get_layer_data(p)
  expect_equal(as.numeric(y$xmin), c(0.55, 1.55))
  expect_equal(as.numeric(y$xmax), c(1.45, 2.45))

  p <- ggplot(dat, aes(x, y)) + geom_col(just = 1.0)
  y <- get_layer_data(p)
  expect_equal(as.numeric(y$xmin), c(0.1, 1.1))
  expect_equal(as.numeric(y$xmax), c(1.0, 2.0))

  p <- ggplot(dat, aes(x, y)) + geom_col(just = 0.0)
  y <- get_layer_data(p)
  expect_equal(as.numeric(y$xmin), c(1.0, 2.0))
  expect_equal(as.numeric(y$xmax), c(1.9, 2.9))
})

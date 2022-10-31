test_that("geom_col removes columns with parts outside the plot limits", {
  dat <- data_frame(x = c(1, 2, 3))

  p <- ggplot(dat, aes(x, x)) + geom_col()

  expect_warning( # warning created at render stage
    ggplotGrob(p + ylim(0.5, 4)),
    "Removed 3 rows containing missing values"
  )
  expect_warning( # warning created at build stage
    ggplot_build(p + ylim(0, 2.5)),
    "Removed 1 rows containing missing values"
  )
})

test_that("geom_col works in both directions", {
  dat <- data_frame(x = c("a", "b", "c"), y = c(1.2, 2.5, 3.1))

  p <- ggplot(dat, aes(x, y)) + geom_col()
  x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(dat, aes(y, x)) + geom_col()
  y <- layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])
})

test_that("geom_col supports alignment of columns", {
  dat <- data_frame(x = c("a", "b"), y = c(1.2, 2.5))

  p <- ggplot(dat, aes(x, y)) + geom_col(just = 0.5)
  y <- layer_data(p)
  expect_equal(as.numeric(y$xmin), c(0.55, 1.55))
  expect_equal(as.numeric(y$xmax), c(1.45, 2.45))

  p <- ggplot(dat, aes(x, y)) + geom_col(just = 1.0)
  y <- layer_data(p)
  expect_equal(as.numeric(y$xmin), c(0.1, 1.1))
  expect_equal(as.numeric(y$xmax), c(1.0, 2.0))

  p <- ggplot(dat, aes(x, y)) + geom_col(just = 0.0)
  y <- layer_data(p)
  expect_equal(as.numeric(y$xmin), c(1.0, 2.0))
  expect_equal(as.numeric(y$xmax), c(1.9, 2.9))
})

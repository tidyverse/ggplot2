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

test_that("geom_bar removes bars with parts outside the plot limits", {
  dat <- data_frame(x = c("a", "b", "b", "c", "c", "c"))

  p <- ggplot(dat, aes(x)) + geom_bar()

  expect_warning( # warning created at render stage
    ggplotGrob(p + ylim(0, 2.5)),
    "Removed 1 rows containing missing values"
  )
})

test_that("geom_bar works in both directions", {
  dat <- data_frame(x = c("a", "b", "b", "c", "c", "c"))

  p <- ggplot(dat, aes(x)) + geom_bar()
  x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(dat, aes(y = x)) + geom_bar()
  y <- layer_data(p)
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

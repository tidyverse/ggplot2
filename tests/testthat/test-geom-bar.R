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

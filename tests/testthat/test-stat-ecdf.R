test_that("stat_ecdf works in both directions", {
  p <- ggplot(mpg, aes(hwy)) + stat_ecdf()
  x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(mpg, aes(y = hwy)) + stat_ecdf()
  y <- layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])

  p <- ggplot(mpg) + stat_ecdf()
  expect_snapshot_error(ggplot_build(p))
})


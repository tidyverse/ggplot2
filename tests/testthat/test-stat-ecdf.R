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

# See #5113 and #5112
test_that("stat_ecdf responds to axis transformations", {
  n <- 4
  answer <- c(seq(0, 1, length.out = n + 1), 1)
  p <- ggplot(data_frame0(x = seq_len(n)), aes(x)) + stat_ecdf()

  ld <- layer_data(p)
  expect_equal(ld$y, answer)

  ld <- layer_data(p + scale_y_sqrt())
  expect_equal(ld$y, sqrt(answer))
})

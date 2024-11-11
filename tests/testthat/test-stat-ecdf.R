test_that("stat_ecdf works in both directions", {
  p <- ggplot(mpg, aes(hwy)) + stat_ecdf()
  x <- get_layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(mpg, aes(y = hwy)) + stat_ecdf()
  y <- get_layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])

  p <- ggplot(mpg) + stat_ecdf()
  expect_snapshot_error(ggplot_build(p))
})

test_that("weighted ecdf computes sensible results", {

  set.seed(42)
  x <- rpois(100, 5)
  ux <- sort(unique0(x))

  # Absent weights should be the same as the original
  expect_equal(
    ecdf(x)(ux),
    wecdf(x, NULL)(ux)
  )

  # Uniform weights should be the same as the original
  expect_equal(
    ecdf(x)(ux),
    wecdf(x, pi)(ux)
  )

  # Tabulated weights should be the same as the original
  tab   <- as.data.frame(table(x), stringsAsFactors = FALSE)
  tab$x <- as.numeric(tab$x)
  expect_equal(
    ecdf(x)(ux),
    wecdf(tab$x, tab$Freq)(ux)
  )
})

test_that("weighted ecdf warns about weird weights", {

  # Should warn when provided with illegal weights
  expect_snapshot_warning(wecdf(1:10, c(NA, rep(1, 9))))

  # Should warn when provided with near-0 weights
  expect_snapshot_warning(wecdf(1:10, .Machine$double.eps))

  # Should error when weights sum to 0
  expect_snapshot(wecdf(1:10, rep(c(-1, 1), 5)), error = TRUE)
})

# See #5113 and #5112
test_that("stat_ecdf responds to axis transformations", {
  n <- 4
  answer <- c(seq(0, 1, length.out = n + 1), 1)
  p <- ggplot(data_frame0(x = seq_len(n)), aes(x)) + stat_ecdf()

  ld <- get_layer_data(p)
  expect_equal(ld$y, answer)

  ld <- get_layer_data(p + scale_y_sqrt())
  expect_equal(ld$y, sqrt(answer))
})

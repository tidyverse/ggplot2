test_that("position scales are updated by all position aesthetics", {
  df <- data_frame(x = 1:3, y = 1:3)

  aesthetics <- list(
    aes(xend = x, yend = x),
    aes(xmin = x, ymin = x),
    aes(xmax = x, ymax = x),
    aes(xintercept = x, yintercept = y)
  )

  base <- ggplot(df, aes(x = 1, y = 1)) + geom_point()
  plots <- lapply(aesthetics, ggplot_add, plot = base)
  ranges <- lapply(plots, pranges)

  lapply(ranges, function(range) {
    expect_equal(range$x[[1]], c(1, 3))
    expect_equal(range$y[[1]], c(1, 3))
  })
})

test_that("oob affects position values", {
  dat <- data_frame(x = c("a", "b", "c"), y = c(1, 5, 10))
  base <- ggplot(dat, aes(x, y)) +
    geom_col() +
    annotate("point", x = "a", y = c(-Inf, Inf))

  y_scale <- function(limits, oob = censor) {
    scale_y_continuous(limits = limits, oob = oob, expand = c(0, 0))
  }
  base + scale_y_continuous(limits = c(-0,5))

  low_censor <- cdata(base + y_scale(c(0, 5), censor))
  mid_censor <- cdata(base + y_scale(c(3, 7), censor))
  handle <- GeomBar$handle_na

  expect_snapshot_warning(
    low_censor[[1]] <- handle(low_censor[[1]], list(na.rm = FALSE)),
  )
  expect_snapshot_warning(
    mid_censor[[1]] <- handle(mid_censor[[1]], list(na.rm = FALSE)),
  )

  low_squish <- cdata(base + y_scale(c(0, 5), squish))
  mid_squish <- cdata(base + y_scale(c(3, 7), squish))

  # Points are always at the top and bottom
  expect_equal(low_censor[[2]]$y, c(0, 1))
  expect_equal(mid_censor[[2]]$y, c(0, 1))
  expect_equal(low_squish[[2]]$y, c(0, 1))
  expect_equal(mid_squish[[2]]$y, c(0, 1))

  # Bars depend on limits and oob
  expect_equal(low_censor[[1]]$y, c(0.2, 1))
  expect_equal(mid_censor[[1]]$y, numeric(0))
  expect_equal(low_squish[[1]]$y, c(0.2, 1, 1))
  expect_equal(mid_squish[[1]]$y, c(0, 0.5, 1))
})

test_that("scales warn when transforms introduces non-finite values", {
  df <- data_frame(x = c(1e1, 1e5), y = c(0, 100))

  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 5) +
    scale_y_log10()

  expect_snapshot_warning(ggplot_build(p))
})

context("geom_violin")

test_that("range is expanded", {
  df <- rbind(
    data.frame(x = "a", y = c(0, runif(10), 1)),
    data.frame(x = "b", y = c(0, runif(10), 2))
  )

  p <- ggplot(df, aes(1, y)) +
    geom_violin(trim = FALSE) +
    facet_grid(x ~ ., scales = "free") +
    coord_cartesian(expand = FALSE)
  expand_a <- stats::bw.nrd0(df$y[df$x == "a"]) * 3
  expand_b <- stats::bw.nrd0(df$y[df$x == "b"]) * 3
  expect_equal(layer_scales(p, 1)$y$dimension(), c(0 - expand_a, 1 + expand_a))
  expect_equal(layer_scales(p, 2)$y$dimension(), c(0 - expand_b, 2 + expand_b))
})

# create_quantile_segment_frame -------------------------------------------------

test_that("create_quantile_segment_frame functions for 3 quantiles", {
  density.data <- data.frame(y = (1:256)/256, density = 1/256) # uniform density

  qs <- c(0.25, 0.5, 0.75) # 3 quantiles
  expect_equal(create_quantile_segment_frame(density.data, qs)$y,
               rep(qs, each = 2))
})

test_that("quantiles do not fail on zero-range data", {
  zero.range.data <- data.frame(y = rep(1,3))
  p <- ggplot(zero.range.data) + geom_violin(aes(1, y), draw_quantiles = 0.5)

  # This should return without error and have length one
  expect_equal(length(layer_grob(p)), 1)
})


# Visual tests ------------------------------------------------------------

test_that("geom_violin draws correctly", {
  set.seed(111)
  dat <- data.frame(x = LETTERS[1:3], y = rnorm(90))
  dat <- dat[dat$x != "C" | c(T, F),]  # Keep half the C's

  expect_doppelganger("basic",
    ggplot(dat, aes(x = x, y = y)) + geom_violin()
  )
  expect_doppelganger("scale area to sample size (C is smaller)",
    ggplot(dat, aes(x = x, y = y)) + geom_violin(scale = "count"),
  )
  expect_doppelganger("narrower (width=.5)",
    ggplot(dat, aes(x = x, y = y)) + geom_violin(width = .5)
  )
  expect_doppelganger("with tails and points",
    ggplot(dat, aes(x = x, y = y)) + geom_violin(trim = FALSE) + geom_point(shape = 21)
  )
  expect_doppelganger("with smaller bandwidth and points",
    ggplot(dat, aes(x = x, y = y)) + geom_violin(adjust = .3) + geom_point(shape = 21)
  )
  expect_doppelganger("dodging",
    ggplot(dat, aes(x = "foo", y = y, fill = x)) + geom_violin()
  )
  expect_doppelganger("coord_polar",
    ggplot(dat, aes(x = x, y = y)) + geom_violin() + coord_polar()
  )
  expect_doppelganger("coord_flip",
    ggplot(dat, aes(x = x, y = y)) + geom_violin() + coord_flip()
  )
  expect_doppelganger("dodging and coord_flip",
    ggplot(dat, aes(x = "foo", y = y, fill = x)) + geom_violin() + coord_flip()
  )
  expect_doppelganger("continuous x axis, multiple groups (center should be at 2.0)",
    ggplot(dat, aes(x = as.numeric(x), y = y)) + geom_violin()
  )
  expect_doppelganger("continuous x axis, single group (center should be at 1.0)",
    ggplot(dat, aes(x = as.numeric(1), y = y)) + geom_violin()
  )
  expect_doppelganger("quantiles",
    ggplot(dat, aes(x=x, y=y)) + geom_violin(draw_quantiles=c(0.25,0.5,0.75))
  )

  dat2 <- data.frame(x = LETTERS[1:3], y = rnorm(90), g = letters[5:6])
  expect_doppelganger("grouping on x and fill",
    ggplot(dat2, aes(x = x, y = y, fill = g)) + geom_violin()
  )
  expect_doppelganger("grouping on x and fill, dodge width = 0.5",
    ggplot(dat2, aes(x = x, y = y, fill = g)) +
      geom_violin(position = position_dodge(width = .5))
  )
})

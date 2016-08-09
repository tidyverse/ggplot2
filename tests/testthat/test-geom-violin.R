context("geom_violin")

test_that("", {
  df <- rbind(
    data.frame(x = "a", y = c(0, runif(10), 1)),
    data.frame(x = "b", y = c(0, runif(10), 2))
  )

  p <- ggplot(df, aes(1, y)) +
    geom_violin() +
    facet_grid(x ~ ., scales = "free") +
    coord_cartesian(expand = FALSE)

  expect_equal(layer_scales(p, 1)$y$dimension(), c(0, 1))
  expect_equal(layer_scales(p, 2)$y$dimension(), c(0, 2))
})

# create_quantile_segment_frame -------------------------------------------------

test_that("create_quantile_segment_frame functions for 3 quantiles", {
  density.data <- data.frame(y=(1:256)/256, density=1/256) # uniform density

  qs <- c(0.25, 0.5, 0.75) # 3 quantiles
  expect_equal(create_quantile_segment_frame(density.data, qs)$y,
               rep(qs, each=2))
})


# Visual tests ------------------------------------------------------------

test_that("geom_violin draws correctly", {
  set.seed(111)
  dat <- data.frame(x = LETTERS[1:3], y = rnorm(90))
  dat <- dat[dat$x != "C" | c(T, F),]  # Keep half the C's

  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(),
    "basic"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(scale = "count"),
    "scale_area_to_sample_size_C_is_smaller"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(width = .5),
    "narrower_width-05"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(trim = FALSE) + geom_point(shape = 21),
    "with_tails_and_points"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin(adjust = .3) + geom_point(shape = 21),
    "with_smaller_bandwidth_and_points"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = "foo", y = y, fill = x)) + geom_violin(),
    "dodging"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin() + coord_polar(),
    "coord_polar"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = x, y = y)) + geom_violin() + coord_flip(),
    "coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = "foo", y = y, fill = x)) + geom_violin() + coord_flip(),
    "dodging_and_coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = as.numeric(x), y = y)) + geom_violin(),
    "continuous_x_axis_multiple_groups_center_should_be_at_2"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x = as.numeric(1), y = y)) + geom_violin(),
    "continuous_x_axis_single_group_center_should_be_at_1"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x=x, y=y)) + geom_violin(draw_quantiles=c(0.25,0.5,0.75)),
    "quantiles"
  )

  dat2 <- data.frame(x = LETTERS[1:3], y = rnorm(90), g = letters[5:6])
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x = x, y = y, fill = g)) + geom_violin(),
    "grouping_on_x_and_fill"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x = x, y = y, fill = g)) +
      geom_violin(position = position_dodge(width = .5)),
    "grouping_on_x_and_fill_dodge_width_05"
  )
})

# thanks wch for providing the test code
test_that("geom_boxplot range includes all outliers", {
  dat <- data_frame(x = 1, y = c(-(1:20) ^ 3, (1:20) ^ 3) )
  p <- ggplot_build(ggplot(dat, aes(x,y)) + geom_boxplot())

  miny <- p$layout$panel_params[[1]]$y.range[1]
  maxy <- p$layout$panel_params[[1]]$y.range[2]

  expect_true(miny <= min(dat$y))
  expect_true(maxy >= max(dat$y))

  # Unless specifically directed not to
  p <- ggplot_build(ggplot(dat, aes(x, y)) + geom_boxplot(outliers = FALSE))

  miny <- p$layout$panel_params[[1]]$y.range[1]
  maxy <- p$layout$panel_params[[1]]$y.range[2]

  expect_lte(maxy, max(dat$y))
  expect_gte(miny, min(dat$y))
})

test_that("geom_boxplot works in both directions", {
  dat <- data_frame(x = 1, y = c(-(1:20) ^ 3, (1:20) ^ 3) )

  p <- ggplot(dat, aes(x, y)) + geom_boxplot()
  x <- get_layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(dat, aes(y, x)) + geom_boxplot()
  y <- get_layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE))
})

test_that("geom_boxplot for continuous x gives warning if more than one x (#992)", {
  dat <- expand.grid(x = 1:2, y = c(-(1:5) ^ 3, (1:5) ^ 3) )

  bplot <- function(aes = NULL, extra = list()) {
    ggplot_build(ggplot(dat, aes) + geom_boxplot(aes) + extra)
  }

  expect_warning(bplot(aes(x, y)), "Continuous x aesthetic")
  expect_warning(bplot(aes(x, y), facet_wrap(~x)), "Continuous x aesthetic")
  expect_warning(bplot(aes(Sys.Date() + x, y)), "Continuous x aesthetic")

  expect_warning(bplot(aes(x, group = x, y)), NA)
  expect_warning(bplot(aes(1, y)), NA)
  expect_warning(bplot(aes(factor(x), y)), NA)
  expect_warning(bplot(aes(x == 1, y)), NA)
  expect_warning(bplot(aes(as.character(x), y)), NA)
})

test_that("can use US spelling of colour", {
  df <- data_frame(x = 1, y = c(1:5, 100))
  plot <- ggplot(df, aes(x, y)) + geom_boxplot(outlier.color = "red")

  gpar <- get_layer_grob(plot)[[1]]$children[[1]]$children[[1]]$gp
  expect_equal(gpar$col, "#FF0000FF")
})

test_that("boxes with variable widths do not overlap", {
  df <- data_frame(
    value = 1:12,
    group = rep(c("a", "b", "c"), each = 4L),
    subgroup = rep(c("A", "B"), times = 6L)
  )

  p <- ggplot(df, aes(group, value, colour = subgroup)) +
    geom_boxplot(varwidth = TRUE)
  d <- get_layer_data(p)[c("xmin", "xmax")]
  xid <- find_x_overlaps(d)

  expect_false(any(duplicated(xid)))
})

test_that("boxplots with a group size >1 error", {
  p <- ggplot(
    data_frame(x = "one value", y = 3, value = 4:6),
    aes(x, ymin = 0, lower = 1, middle = y, upper = value, ymax = 10)
  ) +
    geom_boxplot(stat = "identity")

  expect_equal(nrow(get_layer_data(p, 1)), 3)
  expect_snapshot_error(get_layer_grob(p, 1))
})

# Visual tests ------------------------------------------------------------

test_that("boxplot draws correctly", {
  expect_doppelganger("outlier colours",
    ggplot(mtcars, aes(x = factor(cyl), y = drat, colour = factor(cyl))) + geom_boxplot(outlier.size = 5)
  )
  expect_doppelganger("staples",
    ggplot(mtcars, aes(x = factor(cyl), y = drat, colour = factor(cyl))) + geom_boxplot(staplewidth = 0.5)
  )
})

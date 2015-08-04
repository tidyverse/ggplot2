context("Scales")

test_that("buidling a plot does not affect its scales", {
  dat <- data.frame(x = rnorm(20), y = rnorm(20))

  p <- ggplot(dat, aes(x, y)) + geom_point()
  expect_equal(length(p$scales$scales), 0)

  ggplot_build(p)
  expect_equal(length(p$scales$scales), 0)
})

test_that("ranges update only for variables listed in aesthetics", {
  sc <- scale_alpha()

  scale_train_df(sc, data.frame(alpha = 1:10))
  expect_equal(sc$range$range, c(1, 10))

  scale_train_df(sc, data.frame(alpha = 50))
  expect_equal(sc$range$range, c(1, 50))

  scale_train_df(sc, data.frame(beta = 100))
  expect_equal(sc$range$range, c(1, 50))

  scale_train_df(sc, data.frame())
  expect_equal(sc$range$range, c(1, 50))

})

test_that("mapping works", {
  sc <- scale_alpha(range = c(0, 1), na.value = 0)
  scale_train_df(sc, data.frame(alpha = 1:10))

  expect_equal(
    scale_map_df(sc, data.frame(alpha = 1:10))[[1]],
    seq(0, 1, length.out = 10)
  )

  expect_equal(scale_map_df(sc, data.frame(alpha = NA))[[1]], 0)

  expect_equal(
    scale_map_df(sc, data.frame(alpha = c(-10, 11)))[[1]],
    c(0, 0))
})

test_that("identity scale preserves input values", {
  df <- data.frame(x = 1:3, z = letters[1:3])

  p1 <- ggplot(df,
    aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) +
    geom_point() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_shape_identity() +
    scale_size_identity() +
    scale_alpha_identity()
  d1 <- pdata(p1)[[1]]

  expect_that(d1$colour, equals(as.character(df$z)))
  expect_that(d1$fill, equals(as.character(df$z)))
  expect_that(d1$shape, equals(as.character(df$z)))
  expect_that(d1$size, equals(as.numeric(df$z)))
  expect_that(d1$alpha, equals(as.numeric(df$z)))


})

test_that("position scales updated by all position aesthetics", {
  df <- data.frame(x = 1:3, y = 1:3)

  aesthetics <- list(
    aes(xend = x, yend = x),
    aes(xmin = x, ymin = x),
    aes(xmax = x, ymax = x),
    aes(xintercept = x, yintercept = y)
  )

  base <- ggplot(df, aes(x = 1, y = 1)) + geom_point()
  plots <- lapply(aesthetics, function(x) base %+% x)
  ranges <- lapply(plots, pranges)

  lapply(ranges, function(range) {
    expect_that(range$x[[1]], equals(c(1, 3)))
    expect_that(range$y[[1]], equals(c(1, 3)))
  })

})

test_that("position scales generate after stats", {
  df <- data.frame(x = factor(c(1, 1, 1)))
  plot <- ggplot(df, aes(x)) + geom_bar()
  ranges <- pranges(plot)

  expect_that(ranges$x[[1]], equals(c("1")))
  expect_that(ranges$y[[1]], equals(c(0, 3)))

})

test_that("oob affects position values", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))
  base <- ggplot(dat, aes(x, y)) +
    geom_bar(stat = "identity") +
    annotate("point", x = "a", y = c(-Inf, Inf))

  y_scale <- function(limits, oob = censor) {
    scale_y_continuous(limits = limits, oob = oob, expand = c(0, 0))
  }
  base + scale_y_continuous(limits = c(-0,5))

  expect_warning(low_censor <- cdata(base + y_scale(c(0, 5), censor)),
    "Removed 1 rows containing missing values")
  expect_warning(mid_censor <- cdata(base + y_scale(c(3, 7), censor)),
    "Removed 2 rows containing missing values")

  low_squish <- cdata(base + y_scale(c(0, 5), squish))
  mid_squish <- cdata(base + y_scale(c(3, 7), squish))

  # Points are always at the top and bottom
  expect_equal(low_censor[[2]]$y, c(0, 1))
  expect_equal(mid_censor[[2]]$y, c(0, 1))
  expect_equal(low_squish[[2]]$y, c(0, 1))
  expect_equal(mid_squish[[2]]$y, c(0, 1))

  # Bars depend on limits and oob
  expect_equal(low_censor[[1]]$y, c(0.2, 1))
  expect_equal(mid_censor[[1]]$y, c(0.5))
  expect_equal(low_squish[[1]]$y, c(0.2, 1, 1))
  expect_equal(mid_squish[[1]]$y, c(0, 0.5, 1))


})

test_that("scales looked for in appropriate place", {
  xlabel <- function(x) ggplot_build(x)$panel$x_scales[[1]]$name
  p0 <- qplot(mpg, wt, data = mtcars) + scale_x_continuous("0")
  expect_equal(xlabel(p0), "0")

  scale_x_continuous <- function(...) ggplot2::scale_x_continuous("1")
  p1 <- qplot(mpg, wt, data = mtcars)
  expect_equal(xlabel(p1), "1")

  f <- function() {
    scale_x_continuous <- function(...) ggplot2::scale_x_continuous("2")
    qplot(mpg, wt, data = mtcars)
  }
  p2 <- f()
  expect_equal(xlabel(p2), "2")

  rm(scale_x_continuous)
  p4 <- qplot(mpg, wt, data = mtcars)
  expect_equal(xlabel(p4), waiver())
})

test_that("find_global searches in the right places", {
  testenv <- new.env(parent = globalenv())

  # This should find the scale object in the package environment
  # We can't use expect_identical here because modify_formals changes the
  # underlying object on attach.
  expect_equal(find_global("scale_y_sqrt", testenv),
    ggplot2::scale_y_sqrt)

  # Set an object with the same name in the environment
  testenv$scale_y_sqrt <- "foo"

  # Now it should return the new object
  expect_identical(find_global("scale_y_sqrt", testenv), "foo")

  # If we search in the empty env, we should end up with the object
  # from the ggplot2 namespace
  expect_identical(find_global("scale_y_sqrt", emptyenv()),
    ggplot2::scale_y_sqrt)
})

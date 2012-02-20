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
    round_any(seq(0, 1, length = 10), 1 / 500))
    
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

test_that("scale_breaks with explicit NA options", {
  # X
  sxc <- scale_x_continuous(breaks=NA)
  scale_train(sxc, 1:3)
  expect_identical(scale_breaks(sxc), NA)
  expect_identical(scale_breaks_minor(sxc), NULL)
  
  # Y
  syc <- scale_y_continuous(breaks=NA)
  scale_train(syc, 1:3)
  expect_identical(scale_breaks(syc), NA)
  expect_identical(scale_breaks_minor(syc), NULL)
  
  # Alpha
  sac <- scale_alpha_continuous(breaks=NA)
  scale_train(sac,1:3)
  expect_identical(scale_breaks(sac), NA)
  
  # Size
  ssc <- scale_size_continuous(breaks=NA)
  scale_train(ssc,1:3)
  expect_identical(scale_breaks(ssc), NA)
  
  # Fill
  sfc <- scale_fill_continuous(breaks=NA)
  scale_train(sfc,1:3)
  expect_identical(scale_breaks(sfc), NA)
  
  # Colour
  scc <- scale_colour_continuous(breaks=NA)
  scale_train(scc,1:3)
  expect_identical(scale_breaks(scc), NA)
    
})

test_that("oob affects position values", {
  dat <- data.frame(x=c("a", "b", "c"), y=c(1, 5, 10))
  base <- ggplot(dat, aes(x=x, y=y)) + 
    geom_bar() + 
    annotate("point", x = "a", y = c(-Inf, Inf))

  y_scale <- function(limits, oob = censor) {
    scale_y_continuous(limits = limits, oob = oob, expand = c(0, 0))
  }

  low_censor <- cdata(base + y_scale(c(0, 5), censor))
  mid_censor <- cdata(base + y_scale(c(3, 7), censor))

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

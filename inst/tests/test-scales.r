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

test_that("oob affects position values", {
  dat <- data.frame(x=c("a", "b", "c"), y=c(1, 5, 10))
  base <- ggplot(dat, aes(x=x, y=y)) + 
    geom_bar(stat="identity") +
    annotate("point", x = "a", y = c(-Inf, Inf))

  y_scale <- function(limits, oob = censor) {
    scale_y_continuous(limits = limits, oob = oob, expand = c(0, 0))
  }
  base + scale_y_continuous(limits=c(-0,5))

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
  expect_equal(xlabel(p4), NULL)
})

test_that("find_global searches in the right places", {
  testenv <- new.env(parent = globalenv())

  # This should find the scale object in the package environment
  expect_identical(find_global("scale_colour_hue", testenv),
    ggplot2::scale_colour_hue)

  # Set an object with the same name in the environment
  testenv$scale_colour_hue <- "foo"

  # Now it should return the new object
  expect_identical(find_global("scale_colour_hue", testenv), "foo")

  # If we search in the empty env, we should end up with the object
  # from the ggplot2 namespace
  expect_identical(find_global("scale_colour_hue", emptyenv()),
    ggplot2::scale_colour_hue)
})

test_that("scales clear existing scale", {
  p0 <- qplot(mpg, wt, data = mtcars) + ggplot2::scale_x_continuous(breaks=c(10,20))
  p1 <- p0 + ggplot2::scale_x_continuous(expand=c(1,2),clear=T)
  expect_equal(ggplot_build(p1)$panel$x_scales[[1]]$expand,c(1,2))
  expect_is(ggplot_build(p1)$panel$x_scales[[1]]$breaks, "waiver")
})

test_that("scales merges existing scale", {
  p0 <- qplot(mpg, wt, data = mtcars) + ggplot2::scale_x_continuous(breaks=c(10,20)) 
  p1 <- p0 + ggplot2::scale_x_continuous(expand=c(1,2))
  expect_equal(ggplot_build(p1)$panel$x_scales[[1]]$expand, c(1,2))
  expect_equal(ggplot_build(p1)$panel$x_scales[[1]]$breaks, c(10,20))

  p1 <- p0 + ggplot2::scale_x_continuous(breaks=c(1,2))
  expect_equal(ggplot_build(p1)$panel$x_scales[[1]]$breaks, c(1,2))
})

test_that("sca$is.identical closures",{
  tmp = function(){function(){}}
  tmp_ = function(x){function(){x}}
  tmp2 = function(x){function(){x}}
  tmp3 = function(x){function(n){x+n}}
  tmp4 = function(x,f){a=5;function(n){x+n+f}}
  sca = Scales$new()

  expect_true(sca$is.identical(tmp(),tmp()))
  expect_false(sca$is.identical(tmp2(5),tmp()))

  expect_true(sca$is.identical(tmp_(5),tmp_(5)))
  expect_false(sca$is.identical(tmp_(8),tmp_(5)))
  expect_true(sca$is.identical(tmp2(5),tmp2(5)))
  expect_true(sca$is.identical(tmp3(5),tmp3(5)))

  expect_true(sca$is.identical(tmp4(5),tmp4(5)))
  expect_false(sca$is.identical(tmp4(5),tmp4(8)))
})

test_that("sca$is.identical lists",{
  sca = Scales$new()
  tmp = function(){function(){}}
  tmp2 = function(x){function(){x}}

  expect_true(sca$is.identical(list(a=1,b=tmp()),list(a=1,b=tmp())))
  expect_false(sca$is.identical(list(a=1,b=tmp()),list(a=1,b=tmp2(8))))
  expect_true(sca$is.identical(list(a=1,b=5),list(a=1,b=5)))

  expect_true(sca$is.identical(list(a=1,b=list(x=8)),list(a=1,b=list(x=8))))
  expect_false(sca$is.identical(list(a=1,b=list(x=9)),list(a=1,b=list(x=8))))
})

test_that("sca$is.identical non-closures",{
  sca = Scales$new()
  expect_true(sca$is.identical("ggplot","ggplot"))
  expect_false(sca$is.identical("ggplot2","ggplot"))
  expect_true(sca$is.identical(5,5))
  expect_false(sca$is.identical(6,5))
})

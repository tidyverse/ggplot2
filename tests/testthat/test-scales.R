test_that("building a plot does not affect its scales", {
  dat <- data_frame(x = rnorm(20), y = rnorm(20))

  p <- ggplot(dat, aes(x, y)) + geom_point()
  expect_equal(length(p$scales$scales), 0)

  ggplot_build(p)
  expect_equal(length(p$scales$scales), 0)
})

test_that("ranges update only for variables listed in aesthetics", {
  sc <- scale_alpha()

  sc$train_df(data_frame(alpha = 1:10))
  expect_equal(sc$range$range, c(1, 10))

  sc$train_df(data_frame(alpha = 50))
  expect_equal(sc$range$range, c(1, 50))

  sc$train_df(data_frame(beta = 100))
  expect_equal(sc$range$range, c(1, 50))

  sc$train_df(data_frame())
  expect_equal(sc$range$range, c(1, 50))
})

test_that("mapping works", {
  sc <- scale_alpha(range = c(0, 1), na.value = 0)
  sc$train_df(data_frame(alpha = 1:10))

  expect_equal(
    sc$map_df(data_frame(alpha = 1:10))[[1]],
    seq(0, 1, length.out = 10)
  )

  expect_equal(sc$map_df(data_frame(alpha = NA))[[1]], 0)

  expect_equal(
    sc$map_df(data_frame(alpha = c(-10, 11)))[[1]],
    c(0, 0))
})

test_that("identity scale preserves input values", {
  df <- data_frame(x = 1:3, z = factor(letters[1:3]))

  # aesthetic-specific scales
  p1 <- ggplot(df,
    aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) +
    geom_point() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_shape_identity() +
    scale_size_identity() +
    scale_alpha_identity()
  d1 <- layer_data(p1)

  expect_equal(d1$colour, as.character(df$z))
  expect_equal(d1$fill, as.character(df$z))
  expect_equal(d1$shape, as.character(df$z))
  expect_equal(d1$size, as.numeric(df$z))
  expect_equal(d1$alpha, as.numeric(df$z))

  # generic scales
  p2 <- ggplot(df,
    aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) +
    geom_point() +
    scale_discrete_identity(aesthetics = c("colour", "fill", "shape")) +
    scale_continuous_identity(aesthetics = c("size", "alpha"))
  d2 <- layer_data(p2)

  expect_equal(d1, d2)
})

test_that("position scales are updated by all position aesthetics", {
  df <- data_frame(x = 1:3, y = 1:3)

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
    expect_equal(range$x[[1]], c(1, 3))
    expect_equal(range$y[[1]], c(1, 3))
  })
})

test_that("position scales generate after stats", {
  df <- data_frame(x = factor(c(1, 1, 1)))
  plot <- ggplot(df, aes(x)) + geom_bar()
  ranges <- pranges(plot)

  expect_equal(ranges$x[[1]], c("1"))
  expect_equal(ranges$y[[1]], c(0, 3))
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

  expect_warning(low_censor <- cdata(base + y_scale(c(0, 5), censor)),
    "Removed 1 row containing missing values or values outside the scale range")
  expect_warning(mid_censor <- cdata(base + y_scale(c(3, 7), censor)),
    "Removed 2 rows containing missing values or values outside the scale range")

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

test_that("all-Inf layers are not used for determining the type of scale", {
  d1 <- data_frame(x = c("a", "b"))
  p1 <- ggplot(d1, aes(x, x)) +
    # Inf is numeric, but means discrete values in this case
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "black") +
    geom_point()

  b1 <- ggplot_build(p1)
  expect_s3_class(b1$layout$panel_scales_x[[1]], "ScaleDiscretePosition")

  p2 <- ggplot() +
    # If the layer non-Inf value, it's considered
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "black")

  b2 <- ggplot_build(p2)
  expect_s3_class(b2$layout$panel_scales_x[[1]], "ScaleContinuousPosition")
})

test_that("scales are looked for in appropriate place", {
  xlabel <- function(x) ggplot_build(x)$layout$panel_scales_x[[1]]$name
  p0 <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + scale_x_continuous("0")
  expect_equal(xlabel(p0), "0")

  scale_x_continuous <- function(...) ggplot2::scale_x_continuous("1")
  p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  expect_equal(xlabel(p1), "1")

  f <- function() {
    scale_x_continuous <- function(...) ggplot2::scale_x_continuous("2")
    ggplot(mtcars, aes(mpg, wt)) + geom_point()
  }
  p2 <- f()
  expect_equal(xlabel(p2), "2")

  rm(scale_x_continuous)
  p4 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  expect_equal(xlabel(p4), waiver())
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

test_that("scales warn when transforms introduces non-finite values", {
  df <- data_frame(x = c(1e1, 1e5), y = c(0, 100))

  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 5) +
    scale_y_log10()

  expect_warning(ggplot_build(p), "log-10 transformation introduced infinite values.")
})

test_that("size and alpha scales throw appropriate warnings for factors", {
  df <- data_frame(
    x = 1:3,
    y = 1:3,
    d = LETTERS[1:3],
    o = factor(LETTERS[1:3], ordered = TRUE)
  )
  p <- ggplot(df, aes(x, y))

  # There should be warnings when unordered factors are mapped to size/alpha
  expect_warning(
    ggplot_build(p + geom_point(aes(size = d))),
    "Using size for a discrete variable is not advised."
  )
  expect_warning(
    ggplot_build(p + geom_point(aes(alpha = d))),
    "Using alpha for a discrete variable is not advised."
  )
  expect_warning(
    ggplot_build(p + geom_line(aes(linewidth = d, group = 1))),
    "Using linewidth for a discrete variable is not advised."
  )
  # There should be no warnings for ordered factors
  expect_warning(ggplot_build(p + geom_point(aes(size = o))), NA)
  expect_warning(ggplot_build(p + geom_point(aes(alpha = o))), NA)
})

test_that("shape scale throws appropriate warnings for factors", {
  df <- data_frame(
    x = 1:3,
    y = 1:3,
    d = LETTERS[1:3],
    o = factor(LETTERS[1:3], ordered = TRUE)
  )
  p <- ggplot(df, aes(x, y))

  # There should be no warnings when unordered factors are mapped to shape
  expect_warning(ggplot_build(p + geom_point(aes(shape = d))), NA)

  # There should be warnings for ordered factors
  expect_warning(
    ggplot_build(p + geom_point(aes(shape = o))),
    "Using shapes for an ordinal variable is not advised"
  )
})

test_that("aesthetics can be set independently of scale name", {
  df <- data_frame(
    x = LETTERS[1:3],
    y = LETTERS[4:6]
  )
  p <- ggplot(df, aes(x, y, fill = y)) +
    scale_colour_manual(values = c("red", "green", "blue"), aesthetics = "fill")

  expect_equal(layer_data(p)$fill, c("red", "green", "blue"))
})

test_that("multiple aesthetics can be set with one function call", {
  df <- data_frame(
    x = LETTERS[1:3],
    y = LETTERS[4:6]
  )
  p <- ggplot(df, aes(x, y, colour = x, fill = y)) +
    scale_colour_manual(
      values = c("grey20", "grey40", "grey60", "red", "green", "blue"),
      aesthetics = c("colour", "fill")
    )

  expect_equal(layer_data(p)$colour, c("grey20", "grey40", "grey60"))
  expect_equal(layer_data(p)$fill, c("red", "green", "blue"))

  # color order is determined by data order, and breaks are combined where possible
  df <- data_frame(
    x = LETTERS[1:3],
    y = LETTERS[2:4]
  )
  p <- ggplot(df, aes(x, y, colour = x, fill = y)) +
    scale_colour_manual(
      values = c("cyan", "red", "green", "blue"),
      aesthetics = c("fill", "colour")
    )

  expect_equal(layer_data(p)$colour, c("cyan", "red", "green"))
  expect_equal(layer_data(p)$fill, c("red", "green", "blue"))
})

test_that("limits with NA are replaced with the min/max of the data for continuous scales", {
  make_scale <- function(limits = NULL, data = NULL) {
    scale <- continuous_scale("aesthetic", palette = identity, limits = limits)
    if (!is.null(data)) {
      scale$train(data)
    }
    scale
  }

  # emptiness
  expect_true(make_scale()$is_empty())
  expect_false(make_scale(limits = c(0, 1))$is_empty())
  expect_true(make_scale(limits = c(0, NA))$is_empty())
  expect_true(make_scale(limits = c(NA, NA))$is_empty())
  expect_true(make_scale(limits = c(NA, 0))$is_empty())

  # limits
  expect_equal(make_scale(data = 1:5)$get_limits(), c(1, 5))
  expect_equal(make_scale(limits = c(1, 5))$get_limits(), c(1, 5))
  expect_equal(make_scale(limits = c(NA, NA))$get_limits(), c(0, 1))
  expect_equal(make_scale(limits = c(NA, NA), data = 1:5)$get_limits(), c(1, 5))
  expect_equal(make_scale(limits = c(1, NA), data = 1:5)$get_limits(), c(1, 5))
  expect_equal(make_scale(limits = c(NA, 5), data = 1:5)$get_limits(), c(1, 5))
})

test_that("scale_apply preserves class and attributes", {
  df <- data_frame(
    x = structure(c(1, 2), foo = "bar", class = c("baz", "numeric")),
    y = c(1, 1),
    z = c("A", "B")
  )

  # Functions to make the 'baz'-class more type stable
  `c.baz` <- function(...) {
    dots <- list(...)
    attris <- attributes(dots[[1]])
    x <- do.call("c", lapply(dots, unclass))
    attributes(x) <- attris
    x
  }
  `[.baz` <- function(x, i) {
    attris <- attributes(x)
    x <- unclass(x)[i]
    attributes(x) <- attris
    x
  }

  plot <- ggplot(df, aes(x, y)) +
    scale_x_continuous() +
    # Facetting such that 2 x-scales will exist, i.e. `x` will be subsetted
    facet_grid(~ z, scales = "free_x")
  plot <- ggplot_build(plot)

  # Perform identity transformation via `scale_apply`
  out <- with_bindings(scale_apply(
    df, "x", "transform", 1:2, plot$layout$panel_scales_x
  )[[1]], `c.baz` = `c.baz`, `[.baz` = `[.baz`, .env = global_env())

  # Check that it errors on bad scale ids
  expect_snapshot_error(scale_apply(
    df, "x", "transform", c(NA, 1), plot$layout$panel_scales_x
  ))

  # Check class preservation
  expect_s3_class(out, "baz")
  expect_s3_class(out, "numeric")

  # Check attribute preservation
  expect_identical(attr(out, "foo"), "bar")

  # Negative control: non-type stable classes don't preserve attributes
  class(df$x) <- "foobar"

  out <- with_bindings(scale_apply(
    df, "x", "transform", 1:2, plot$layout$panel_scales_x
  )[[1]], `c.baz` = `c.baz`, `[.baz` = `[.baz`, .env = global_env())

  expect_false(inherits(out, "foobar"))
  expect_null(attributes(out))
})

test_that("All scale_colour_*() have their American versions", {
  # In testthat, the package env contains non-exported functions as well so we
  # need to parse NAMESPACE file by ourselves
  exports <- readLines(system.file("NAMESPACE", package = "ggplot2"))
  colour_scale_exports <- grep("export\\(scale_colour_.*\\)", exports, value = TRUE)
  color_scale_exports <- grep("export\\(scale_color_.*\\)", exports, value = TRUE)
  expect_equal(
    colour_scale_exports,
    sub("color", "colour", color_scale_exports)
  )
})

test_that("scales accept lambda notation for function input", {
  check_lambda <- function(items, ggproto) {
    vapply(items, function(x) {
      f <- environment(ggproto[[x]])$f
      is_lambda(f)
    }, logical(1))
  }

  # Test continuous scale
  scale <- scale_fill_gradient(
    limits = ~ .x + c(-1, 1),
    breaks = ~ seq(.x[1], .x[2], by = 2),
    minor_breaks = ~ seq(.x[1], .x[2], by = 1),
    labels = ~ toupper(.x),
    rescaler = ~ rescale_mid(.x, mid = 0),
    oob = ~ oob_squish(.x, .y, only.finite = FALSE)
  )
  check <- check_lambda(
    c("limits", "breaks", "minor_breaks", "labels", "rescaler"),
    scale
  )
  expect_true(all(check))

  # Test discrete scale
  scale <- scale_x_discrete(
    limits = ~ rev(.x),
    breaks = ~ .x[-1],
    labels = ~ toupper(.x)
  )
  check <- check_lambda(c("limits", "breaks", "labels"), scale)
  expect_true(all(check))

  # Test binned scale
  scale <- scale_fill_steps(
    limits = ~ .x + c(-1, 1),
    breaks = ~ seq(.x[1], .x[2], by = 2),
    labels = ~ toupper(.x),
    rescaler = ~ rescale_mid(.x, mid = 0),
    oob = ~ oob_squish(.x, .y, only.finite = FALSE)
  )
  check <- check_lambda(
    c("limits", "breaks", "labels", "rescaler"),
    scale
  )
  expect_true(all(check))
})

test_that("breaks and labels are correctly checked", {
  expect_snapshot_error(check_breaks_labels(1:10, letters))
  p <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + scale_x_continuous(breaks = NA)
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + scale_x_continuous(minor_breaks = NA)
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + scale_x_continuous(labels = NA)
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + scale_x_continuous(labels = function(x) 1:2)
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) + geom_bar(aes(factor(gear))) + scale_x_discrete(breaks = NA)
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_bar(aes(factor(gear))) + scale_x_discrete(labels = NA)
  expect_snapshot_error(ggplotGrob(p))

  p <- ggplot(mtcars) + geom_bar(aes(mpg)) + scale_x_binned(breaks = NA)
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_bar(aes(mpg)) + scale_x_binned(labels = NA)
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) + geom_bar(aes(mpg)) + scale_x_binned(labels = function(x) 1:2)
  expect_snapshot_error(ggplotGrob(p))
})

test_that("staged aesthetics are backtransformed properly (#4155)", {
  p <- ggplot(data.frame(value = 16)) +
    geom_point(aes(stage(value, after_stat = x / 2), 0)) +
    scale_x_sqrt(limits = c(0, 16), breaks = c(2, 4, 8))

  # x / 2 should be 16 / 2 = 8, thus the result should be sqrt(8) on scale_x_sqrt()
  expect_equal(layer_data(p)$x, sqrt(8))
})

test_that("numeric scale transforms can produce breaks", {

  test_breaks <- function(trans, limits) {
    scale <- scale_x_continuous(trans = trans)
    scale$train(scale$transform(limits))
    view <- view_scale_primary(scale)
    scale$trans$inverse(view$get_breaks())
  }

  expect_equal(test_breaks("asn", limits = c(0, 1)),
               seq(0, 1, by = 0.25))

  expect_equal(test_breaks("sqrt", limits = c(0, 10)),
               seq(0, 10, by = 2.5))

  expect_equal(test_breaks("atanh", limits = c(-0.9, 0.9)),
               c(NA, -0.5, 0, 0.5, NA))

  expect_equal(test_breaks(transform_boxcox(0), limits = c(1, 10)),
               c(NA, 2.5, 5.0, 7.5, 10))

  expect_equal(test_breaks(transform_modulus(0), c(-10, 10)),
               seq(-10, 10, by = 5))

  expect_equal(test_breaks(transform_yj(0), c(-10, 10)),
               seq(-10, 10, by = 5))

  expect_equal(test_breaks("exp", c(-10, 10)),
               seq(-10, 10, by = 5))

  expect_equal(test_breaks("identity", limits = c(-10, 10)),
               seq(-10, 10, by = 5))

  # irrational numbers, so snapshot values
  expect_snapshot(test_breaks("log", limits = c(0.1, 1000)))

  expect_equal(test_breaks("log10", limits = c(0.1, 1000)),
               10 ^ seq(-1, 3))

  expect_equal(test_breaks("log2", limits = c(0.5, 32)),
               c(0.5, 2, 8, 32))

  expect_equal(test_breaks("log1p", limits = c(0, 10)),
               seq(0, 10, by = 2.5))

  expect_equal(test_breaks("pseudo_log", limits = c(-10, 10)),
               seq(-10, 10, by = 5))

  expect_equal(test_breaks("logit", limits = c(0.001, 0.999)),
               c(NA, 0.25, 0.5, 0.75, NA))

  expect_equal(test_breaks("probit", limits = c(0.001, 0.999)),
               c(NA, 0.25, 0.5, 0.75, NA))

  expect_equal(test_breaks("reciprocal", limits = c(1, 10)),
               c(NA, 2.5, 5, 7.5, 10))

  expect_equal(test_breaks("reverse", limits = c(-10, 10)),
               seq(-10, 10, by = 5))

  expect_equal(test_breaks("sqrt", limits = c(0, 10)),
               seq(0, 10, by = 2.5))
})

test_that("scale functions accurately report their calls", {

  construct <- exprs(
    scale_alpha(),
    scale_alpha_binned(),
    scale_alpha_continuous(),
    scale_alpha_date(),
    scale_alpha_datetime(),
    scale_alpha_discrete(),
    scale_alpha_identity(),
    scale_alpha_manual(),
    scale_alpha_ordinal(),
    # Skipping American spelling of 'color' scales here
    scale_colour_binned(),
    scale_colour_brewer(),
    scale_colour_continuous(),
    scale_colour_date(),
    scale_colour_datetime(),
    scale_colour_discrete(),
    scale_colour_distiller(),
    scale_colour_fermenter(),
    scale_colour_gradient(),
    scale_colour_gradient2(),
    # Some scales have required arguments
    scale_colour_gradientn(colours = c("firebrick", "limegreen")),
    scale_colour_grey(),
    scale_colour_hue(),
    scale_colour_identity(),
    scale_colour_manual(),
    scale_colour_ordinal(),
    scale_colour_steps(),
    scale_colour_steps2(),
    scale_colour_stepsn(colours = c("orchid", "tomato")),
    scale_colour_viridis_b(),
    scale_colour_viridis_c(),
    scale_colour_viridis_d(),
    scale_continuous_identity(aesthetics = "foo"),
    scale_discrete_identity(aesthetics = "bar"),
    scale_discrete_manual(aesthetics = "baz"),
    scale_fill_binned(),
    scale_fill_brewer(),
    scale_fill_continuous(),
    scale_fill_date(),
    scale_fill_datetime(),
    scale_fill_discrete(),
    scale_fill_distiller(),
    scale_fill_fermenter(),
    scale_fill_gradient(),
    scale_fill_gradient2(),
    scale_fill_gradientn(colours = c("yellow", "green")),
    scale_fill_grey(),
    scale_fill_hue(),
    scale_fill_identity(),
    scale_fill_manual(),
    scale_fill_ordinal(),
    scale_fill_steps(),
    scale_fill_steps2(),
    scale_fill_stepsn(colours = c("steelblue", "pink")),
    scale_fill_viridis_b(),
    scale_fill_viridis_c(),
    scale_fill_viridis_d(),
    scale_linetype(),
    scale_linetype_binned(),
    # scale_linetype_continuous(), # designed to throw error
    scale_linetype_discrete(),
    scale_linetype_identity(),
    scale_linetype_manual(),
    scale_linewidth(),
    scale_linewidth_binned(),
    scale_linewidth_continuous(),
    scale_linewidth_date(),
    scale_linewidth_datetime(),
    scale_linewidth_discrete(),
    scale_linewidth_identity(),
    scale_linewidth_manual(),
    scale_linewidth_ordinal(),
    scale_radius(),
    scale_shape(),
    scale_shape_binned(),
    # scale_shape_continuous(), # designed to throw error
    scale_shape_discrete(),
    scale_shape_identity(),
    scale_shape_manual(),
    scale_shape_ordinal(),
    scale_size(),
    scale_size_area(),
    scale_size_binned(),
    scale_size_binned_area(),
    scale_size_continuous(),
    scale_size_date(),
    scale_size_datetime(),
    scale_size_discrete(),
    scale_size_identity(),
    scale_size_manual(),
    scale_size_ordinal(),
    scale_x_binned(),
    scale_x_continuous(),
    scale_x_date(),
    scale_x_datetime(),
    scale_x_discrete(),
    scale_x_log10(),
    scale_x_reverse(),
    scale_x_sqrt(),
    # scale_x_time(),
    scale_y_binned(),
    scale_y_continuous(),
    scale_y_date(),
    scale_y_datetime(),
    scale_y_discrete(),
    scale_y_log10(),
    scale_y_reverse(),
    scale_y_sqrt(),
    # scale_y_time(),
    xlim(10, 20),
    ylim("A", "B")
  )
  if (is_installed("hms")) {
    construct <- c(construct, exprs(scale_x_time(), scale_y_time()))
  }

  suppressWarnings(
    calls <- lapply(construct, function(x) eval(x)$call)
  )
  expect_equal(calls, construct)
})

test_that("scale call is found accurately", {

  call_template <- quote(scale_x_continuous(trans = "log10"))

  sc <- do.call("scale_x_continuous", list(trans = "log10"))
  expect_equal(sc$call, call_template)

  sc <- inject(scale_x_continuous(!!!list(trans = "log10")))
  expect_equal(sc$call, call_template)

  sc <- exec("scale_x_continuous", trans = "log10")
  expect_equal(sc$call, call_template)

  foo <- function() scale_x_continuous(trans = "log10")
  expect_equal(foo()$call, call_template)

  env <- new_environment()
  env$bar <- function() scale_x_continuous(trans = "log10")
  expect_equal(env$bar()$call, call_template)

  # Now should recognise the outer function
  scale_x_new <- function() {
    scale_x_continuous(trans = "log10")
  }
  expect_equal(
    scale_x_new()$call,
    quote(scale_x_new())
  )
})

test_that("training incorrectly appropriately communicates the offenders", {

  sc <- scale_colour_viridis_d()
  expect_snapshot_error(
    sc$train(1:5)
  )

  sc <- scale_colour_viridis_c()
  expect_snapshot_error(
    sc$train(LETTERS[1:5])
  )
})

test_that("find_scale appends appropriate calls", {

  expect_equal(
    find_scale("x", 1)$call,
    quote(scale_x_continuous())
  )

  expect_equal(
    find_scale("colour", "A")$call,
    quote(scale_colour_discrete())
  )

})

test_that("Using `scale_name` prompts deprecation message", {

  expect_snapshot_warning(continuous_scale("x", "foobar", pal_identity()))
  expect_snapshot_warning(discrete_scale("x",   "foobar", pal_identity()))
  expect_snapshot_warning(binned_scale("x",     "foobar", pal_identity()))

})

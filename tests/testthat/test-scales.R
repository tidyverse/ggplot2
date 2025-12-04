test_that("building a plot does not affect its scales", {
  dat <- data_frame(x = rnorm(20), y = rnorm(20))

  p <- ggplot(dat, aes(x, y)) + geom_point()
  expect_length(p@scales$scales, 0)

  ggplot_build(p)
  expect_length(p@scales$scales, 0)
})

test_that("position scales generate after stats", {
  df <- data_frame(x = factor(c(1, 1, 1)))
  plot <- ggplot(df, aes(x)) + geom_bar()
  ranges <- pranges(plot)

  expect_equal(ranges$x[[1]], c("1"))
  expect_equal(ranges$y[[1]], c(0, 3))
})

test_that("all-Inf layers are not used for determining the type of scale", {
  d1 <- data_frame(x = c("a", "b"))
  p1 <- ggplot(d1, aes(x, x)) +
    # Inf is numeric, but means discrete values in this case
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "black") +
    geom_point()

  b1 <- ggplot_build(p1)
  expect_s3_class(b1@layout$panel_scales_x[[1]], "ScaleDiscretePosition")

  p2 <- ggplot() +
    # If the layer non-Inf value, it's considered
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "black")

  b2 <- ggplot_build(p2)
  expect_s3_class(b2@layout$panel_scales_x[[1]], "ScaleContinuousPosition")
})

test_that("scales are looked for in appropriate place", {
  xlabel <- function(x) ggplot_build(x)@layout$panel_scales_x[[1]]$name
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

test_that("size and alpha scales throw appropriate warnings for factors", {
  df <- data_frame(
    x = 1:3,
    y = 1:3,
    d = LETTERS[1:3],
    o = factor(LETTERS[1:3], ordered = TRUE)
  )
  p <- ggplot(df, aes(x, y))

  # There should be warnings when unordered factors are mapped to size/alpha
  expect_snapshot_warning(
    ggplot_build(p + geom_point(aes(size = d)))
  )
  expect_snapshot_warning(
    ggplot_build(p + geom_point(aes(alpha = d)))
  )
  expect_snapshot_warning(
    ggplot_build(p + geom_line(aes(linewidth = d, group = 1)))
  )
  # There should be no warnings for ordered factors
  expect_no_warning(ggplot_build(p + geom_point(aes(size = o))))
  expect_no_warning(ggplot_build(p + geom_point(aes(alpha = o))))
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
  expect_no_warning(ggplot_build(p + geom_point(aes(shape = d))))

  # There should be warnings for ordered factors
  expect_snapshot_warning(
    ggplot_build(p + geom_point(aes(shape = o)))
  )
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
    df, "x", "transform", 1:2, plot@layout$panel_scales_x
  )[[1]], `c.baz` = `c.baz`, `[.baz` = `[.baz`, .env = global_env())

  # Check that it errors on bad scale ids
  expect_snapshot_error(scale_apply(
    df, "x", "transform", c(NA, 1), plot@layout$panel_scales_x
  ))

  # Check class preservation
  expect_s3_class(out, "baz")
  expect_s3_class(out, "numeric")

  # Check attribute preservation
  expect_identical(attr(out, "foo"), "bar")

  # Negative control: non-type stable classes don't preserve attributes
  class(df$x) <- "foobar"

  out <- with_bindings(scale_apply(
    df, "x", "transform", 1:2, plot@layout$panel_scales_x
  )[[1]], `c.baz` = `c.baz`, `[.baz` = `[.baz`, .env = global_env())

  expect_false(inherits(out, "foobar"))
  expect_null(attributes(out))
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

test_that("populating palettes works", {

  scl <- scales_list()
  scl$add(scale_colour_discrete(aesthetics = c("colour", "fill")))

  my_theme <- theme(
    palette.colour.discrete = c("white", "black"),
    palette.fill.discrete = c("red", "blue")
  )

  scl$set_palettes(my_theme)
  expect_equal(scl$scales[[1]]$palette(2), c("white", "black"))

  # Scales with >1 aesthetic
  scl <- scales_list()
  scl$add(scale_colour_discrete(aesthetics = c("colour", "fill")))

  my_theme$palette.colour.discrete <- NULL

  scl$set_palettes(my_theme)
  expect_equal(scl$scales[[1]]$palette(2), c("red", "blue"))

})

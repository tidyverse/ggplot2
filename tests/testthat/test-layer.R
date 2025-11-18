# Parameters --------------------------------------------------------------

test_that("layer() checks its input", {
  expect_snapshot_error(layer(stat = "identity", position = "identity"))
  expect_snapshot_error(layer(geom = "point", position = "identity"))
  expect_snapshot_error(layer(geom = "point", stat = "identity"))

  expect_snapshot_error(layer("point", "identity", mapping = 1:4, position = "identity"))
  expect_snapshot_error(layer("point", "identity", mapping = ggplot(), position = "identity"))

  expect_snapshot_error(validate_subclass("test", "geom"))
  expect_snapshot_error(validate_subclass(environment(), "geom"))

  geom_foo <- function(...) stop("This function is unconstructable.")
  expect_snapshot_error(layer("foo", "identity", position = "identity"))
})

test_that("aesthetics go in aes_params", {
  l <- geom_point(size = "red")
  expect_equal(l$aes_params, list(size = "red"))
})

test_that("unknown params create warning", {
  expect_snapshot_warning(geom_point(blah = "red"))
})

test_that("unknown aesthetics create warning", {
  expect_snapshot_warning(geom_point(aes(blah = "red")))
})

test_that("empty aesthetics create warning", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg), fill = NULL, shape = character())
  expect_snapshot_warning(ggplot_build(p))
})

test_that("invalid aesthetics throws errors", {
  # We want to test error and ignore the scale search message
  suppressMessages({
    p <- ggplot(mtcars) + geom_point(aes(disp, mpg, fill = data))
    expect_snapshot_error(ggplot_build(p))
    p <- ggplot(mtcars) + geom_point(aes(disp, mpg, fill = after_stat(data)))
    expect_snapshot_error(ggplot_build(p))
  })
})

test_that("unknown NULL aesthetic doesn't create warning (#1909)", {
  expect_silent(geom_point(aes(blah = NULL)))
})

test_that("column vectors are allowed (#2609)", {
  df <- data_frame(x = 1:10)
  df$y <- scale(1:10) # Returns a column vector
  p <- ggplot(df, aes(x, y))
  expect_s3_class(get_layer_data(p), "data.frame")
})

test_that("missing aesthetics trigger informative error", {
  df <- data_frame(x = 1:10)
  expect_snapshot(
    ggplot_build(ggplot(df) + geom_line()),
    error = TRUE
  )
  expect_snapshot(
    ggplot_build(ggplot(df) + geom_col()),
    error = TRUE
  )
})

test_that("function aesthetics are wrapped with after_stat()", {
  df <- data_frame(x = 1:10)
  suppressMessages(
    expect_snapshot_error(
      ggplot_build(
        ggplot(df, aes(colour = density, fill = density)) + geom_point()
      )
    )
  )
})

test_that("computed stats are in appropriate layer", {
  df <- data_frame(x = 1:10)
  expect_snapshot_error(
    ggplot_build(ggplot(df, aes(colour = after_stat(density), fill = after_stat(density))) + geom_point())
  )
})

test_that("if an aes is mapped to a function that returns NULL, it is removed", {
  df <- data_frame(x = 1:10)
  null <- function(...) NULL
  p <- cdata(ggplot(df, aes(x, null())))
  expect_named(p[[1]], c("x", "PANEL", "group"))
})

test_that("layers are stateless except for the computed params", {
  df <- data.frame(x = 1:10, y = 1:10)
  p <- ggplot(df) +
    geom_col(aes(x = x, y = y), width = 0.8, fill = "red")
  col_layer <- as.list(p@layers[[1]])
  stateless_names <- setdiff(names(col_layer), c("computed_geom_params", "computed_stat_params", "computed_mapping"))
  invisible(ggplotGrob(p))
  expect_identical(as.list(p@layers[[1]])[stateless_names], col_layer[stateless_names])
})

test_that("inherit.aes works", {
  df <- data.frame(x = 1:10, y = 1:10)
  p1 <- ggplot(df, aes(y = y)) +
    geom_col(aes(x = x), inherit.aes = TRUE)
  p2 <- ggplot(df, aes(colour = y)) +
    geom_col(aes(x = x, y = y), inherit.aes = FALSE)
  invisible(ggplotGrob(p1))
  invisible(ggplotGrob(p2))
  expect_identical(p1@layers[[1]]$computed_mapping, p2@layers[[1]]$computed_mapping)
})

test_that("retransform works on computed aesthetics in `map_statistic`", {
  df <- data.frame(x = rep(c(1,2), c(9, 25)))
  p <- ggplot(df, aes(x)) + geom_bar() + scale_y_sqrt()
  expect_equal(get_layer_data(p)$y, c(3, 5))

  # To double check: should be original values when `retransform = FALSE`
  parent <- p@layers[[1]]$stat
  p@layers[[1]]$stat <- ggproto(NULL, parent, retransform = FALSE)
  expect_equal(get_layer_data(p)$y, c(9, 25))
})

test_that("layer reports the error with correct index etc", {
  p <- ggplot(mtcars) + geom_linerange(aes(disp, mpg), ymin = 2)

  expect_snapshot_error(ggplotGrob(p))

  p <- ggplot(
    data_frame(x = "one value", y = 3, value = 4:6),
    aes(x, ymin = 0, lower = 1, middle = y, upper = value, ymax = 10)
  ) +
    geom_point(aes(x = x, y = y), inherit.aes = FALSE) +
    geom_boxplot(stat = "identity")

  expect_snapshot_error(ggplotGrob(p))
})

test_that("layer warns for constant aesthetics", {
  p <- ggplot(mtcars, aes(x = seq_along(mpg))) + geom_point(aes(y = 2))
  expect_silent(ggplot_build(p))

  p <- ggplot(mtcars, aes(x = 1)) + geom_point(aes(y = 2))
  expect_snapshot_warning(ggplot_build(p))
})

test_that("layer names can be resolved", {

  p <- ggplot() + geom_point() + geom_point()
  expect_named(p@layers, c("geom_point", "geom_point...2"))

  p <- ggplot() + geom_point(name = "foo") + geom_point(name = "bar")
  expect_named(p@layers, c("foo", "bar"))

  l <- geom_point(name = "foobar")
  expect_snapshot(p + l + l, error = TRUE)
})

test_that("validate_subclass can resolve classes via constructors", {

  env <- new_environment(list(
    geom_foobar = geom_point,
    stat_foobar = stat_boxplot,
    position_foobar = position_nudge,
    guide_foobar = guide_axis_theta
  ))

  expect_s3_class(validate_subclass("foobar", "Geom", env = env), "GeomPoint")
  expect_s3_class(validate_subclass("foobar", "Stat", env = env), "StatBoxplot")
  expect_s3_class(validate_subclass("foobar", "Position", env = env), "PositionNudge")
  expect_s3_class(validate_subclass("foobar", "Guide", env = env), "GuideAxisTheta")

})

test_that("attributes on layer data are preserved", {
  # This is a good layer for testing because:
  # * It needs to compute a statistic at the group level
  # * It needs to setup data to reshape x/y/width/height into xmin/xmax/ymin/ymax
  # * It needs to use a position adjustment
  # * It has an `after_stat()` so it enters the map_statistic method
  old <- stat_summary(
    aes(fill = after_stat(y)),
    fun = mean, geom = "col", position = "dodge"
  )
  # We modify the compute aesthetics method to append a test attribute
  new <- ggproto(NULL, old, compute_aesthetics = function(self, data, plot) {
    data <- ggproto_parent(old, self)$compute_aesthetics(data, plot)
    attr(data, "test") <- "preserve me"
    data
  })
  # At the end of plot building, we want to retrieve that metric
  ld <- layer_data(
    ggplot(mpg, aes(drv, hwy, colour = factor(year))) + new + facet_grid(~year) +
      scale_y_sqrt()
  )
  expect_equal(attr(ld, "test"), "preserve me")
})

# Data extraction ---------------------------------------------------------

test_that("AsIs data passes unmodified", {
  p <- ggplot() + geom_blank(aes(x = 1:2, y = 1:2))
  ld <- get_layer_data(p + geom_point(aes(x = I(0.5), y = I(0.5))), 2)
  expect_s3_class(ld$x, "AsIs")
  expect_equal(ld$y, I(0.5))
  ld <- get_layer_data(p + geom_point(x = I(0.5), y = I(0.5), data = mtcars), 2)
  expect_s3_class(ld$x, "AsIs")
  expect_equal(ld$y[1], I(0.5))
  ld <- get_layer_data(p + annotate("point", x = I(0.5), y = I(0.5)), 2)
  expect_s3_class(ld$x, "AsIs")
  expect_equal(ld$y, I(0.5))
})

test_that("layer_data returns a data.frame", {
  l <- geom_point()
  expect_equal(l$layer_data(mtcars), unrowname(mtcars))
  l <- geom_point(data = head(mtcars))
  expect_equal(l$layer_data(mtcars), head(unrowname(mtcars)))
  l <- geom_point(data = head)
  expect_equal(l$layer_data(mtcars), head(unrowname(mtcars)))
  l <- geom_point(data = ~ head(., 10))
  expect_equal(l$layer_data(mtcars), head(unrowname(mtcars), 10))
  l <- geom_point(data = nrow)
  expect_snapshot_error(l$layer_data(mtcars))
})

test_that("get_layer_data works with layer names", {
  p <- ggplot() +
    annotate("point", x = 1, y = 1, name = "foo") +
    annotate("line", x = 1:2, y = 1:2, name = "bar")

  # name has higher precedence than index
  expect_identical(
    get_layer_data(p, i = "bar"),
    get_layer_data(p, i = 2L)
  )
})

test_that("get_layer_grob works with layer names", {
  p <- ggplot() +
    annotate("point", x = 1, y = 1, name = "foo") +
    annotate("line", x = 1:2, y = 1:2, name = "bar")

  # name has higher precedence than index
  named <- get_layer_grob(p, i = "bar")
  nummed <- get_layer_grob(p, i = 2L)
  named[[1]]$name <- nummed[[1]]$name <- NULL # ignore grid's unique names
  expect_identical(named, nummed)
})

test_that("data.frames and matrix aesthetics survive the build stage", {
  df <- data_frame0(
    x = 1:2,
    g = matrix(1:4, 2),
    f = data_frame0(a = 1:2, b = c("c", "d"))
  )

  p <- layer_data(
    ggplot(df, aes(x, x, colour = g, shape = f)) +
      geom_point() +
      scale_colour_identity() +
      scale_shape_identity()
  )
  expect_vector(p$colour, matrix(NA_integer_, nrow = 0, ncol = 2), size = 2)
  expect_vector(p$shape,  data_frame0(a = integer(), b = character()), size = 2)
})

# Empty data --------------------------------------------------------------

df0 <- data_frame(mpg = numeric(0), wt = numeric(0), am = numeric(0), cyl = numeric(0))

test_that("layers with empty data are silently omitted", {
  # Empty data (no visible points)
  d <- ggplot(df0, aes(mpg,wt)) + geom_point()
  expect_equal(nrow(get_layer_data(d)), 0)

  d <- ggplot() + geom_point(data = df0, aes(mpg,wt))
  expect_equal(nrow(get_layer_data(d)), 0)

  # Regular mtcars data, x=mpg, y=wt, normal points and points from empty data frame
  d <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + geom_point(data = df0)
  expect_equal(nrow(get_layer_data(d, 1)), nrow(mtcars))
  expect_equal(nrow(get_layer_data(d, 2)), 0)

  # Regular mtcars data, but points only from empty data frame
  d <- ggplot(mtcars, aes(mpg, wt)) + geom_point(data = df0)
  expect_equal(nrow(get_layer_data(d, 1)), 0)
})

test_that("plots with empty data and vectors for aesthetics work", {
  d <- ggplot(NULL, aes(1:5, 1:5)) + geom_point()
  expect_equal(nrow(get_layer_data(d)), 5)

  d <- ggplot(data_frame(), aes(1:5, 1:5)) + geom_point()
  expect_equal(nrow(get_layer_data(d)), 5)

  d <- ggplot() + geom_point(aes(1:5, 1:5))
  expect_equal(nrow(get_layer_data(d)), 5)
})

test_that("layers with empty data are silently omitted with facet_wrap", {
  # Empty data, facet_wrap, throws error
  d <- ggplot(df0, aes(mpg, wt)) +
    geom_point() +
    facet_wrap(~cyl)
  expect_snapshot(get_layer_data(d), error = TRUE)

  d <- d + geom_point(data = mtcars)
  expect_equal(nrow(get_layer_data(d, 1)), 0)
  expect_equal(nrow(get_layer_data(d, 2)), nrow(mtcars))
})

test_that("layers with empty data are silently omitted with facet_grid", {
  d <- ggplot(df0, aes(mpg, wt)) +
    geom_point() +
    facet_grid(am ~ cyl)
  expect_snapshot(get_layer_data(d), error = TRUE)

  d <- d + geom_point(data = mtcars)
  expect_equal(nrow(get_layer_data(d, 1)), 0)
  expect_equal(nrow(get_layer_data(d, 2)), nrow(mtcars))
})

test_that("empty data overrides plot defaults", {
  # No extra points when x and y vars don't exist but are set
  d <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    geom_point(data = data_frame(), x = 20, y = 3)
  expect_equal(nrow(get_layer_data(d, 1)), nrow(mtcars))
  expect_equal(nrow(get_layer_data(d, 2)), 0)

  # No extra points when x and y vars are empty, even when aesthetics are set
  d <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    geom_point(data = df0, x = 20, y = 3)
  expect_equal(nrow(get_layer_data(d, 1)), nrow(mtcars))
  expect_equal(nrow(get_layer_data(d, 2)), 0)

  skip_if(getRversion() <= "4.4.0")
  d <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    geom_point(data = data_frame())
  expect_snapshot(get_layer_data(d), error = TRUE)
})

test_that("layer inherits data from plot when data = NULL", {
  d <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point(data = NULL)
  expect_equal(nrow(get_layer_data(d)), nrow(mtcars))
})

test_that("empty layers still generate one grob per panel", {
  df <- data_frame(x = 1:3, y = c("a", "b", "c"))

  d <- ggplot(df, aes(x, y)) +
    geom_point(data = df[0, ]) +
    geom_point() +
    facet_wrap(~y)

  expect_length(get_layer_grob(d), 3)
})

test_that("missing layers generate one grob per panel", {
  df <- data_frame(x = 1:4, y = rep(1:2, 2), g = rep(1:2, 2))
  base <- ggplot(df, aes(x, y)) + geom_point(shape = NA, na.rm = TRUE)

  expect_length(get_layer_grob(base), 1)
  expect_length(get_layer_grob(base + facet_wrap(~ g)), 2)
})



# Parameters --------------------------------------------------------------

test_that("layer() checks its input", {
  expect_snapshot_error(layer(stat = "identity", position = "identity"))
  expect_snapshot_error(layer(geom = "point", position = "identity"))
  expect_snapshot_error(layer(geom = "point", stat = "identity"))

  expect_snapshot_error(layer("point", "identity", mapping = 1:4, position = "identity"))
  expect_snapshot_error(layer("point", "identity", mapping = ggplot(), position = "identity"))

  expect_snapshot_error(check_subclass("test", "geom"))
  expect_snapshot_error(check_subclass(environment(), "geom"))
})

test_that("aesthetics go in aes_params", {
  l <- geom_point(size = "red")
  expect_equal(l$aes_params, list(size = "red"))
})

test_that("unknown params create warning", {
  expect_warning(geom_point(blah = "red"), "unknown parameters")
})

test_that("unknown aesthietcs create warning", {
  expect_warning(geom_point(aes(blah = "red")), "unknown aesthetics")
})

test_that("invalid aesthetics throws errors", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, fill = data))
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, fill = after_stat(data)))
  expect_snapshot_error(ggplot_build(p))
})

test_that("unknown NULL asthetic doesn't create warning (#1909)", {
  expect_warning(geom_point(aes(blah = NULL)), NA)
})

test_that("column vectors are allowed (#2609)", {
  df <- data_frame(x = 1:10)
  df$y <- scale(1:10) # Returns a column vector
  p <- ggplot(df, aes(x, y))
  expect_s3_class(layer_data(p), "data.frame")
})

test_that("missing aesthetics trigger informative error", {
  df <- data_frame(x = 1:10)
  expect_error(
    ggplot_build(ggplot(df) + geom_line()),
    "requires the following missing aesthetics:"
  )
  expect_error(
    ggplot_build(ggplot(df) + geom_col()),
    "requires the following missing aesthetics:"
  )
})

test_that("function aesthetics are wrapped with after_stat()", {
  df <- data_frame(x = 1:10)
  expect_snapshot_error(
    ggplot_build(ggplot(df, aes(colour = density, fill = density)) + geom_point())
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
  expect_identical(names(p[[1]]), c("x", "PANEL", "group"))
})

test_that("layers are stateless except for the computed params", {
  df <- data.frame(x = 1:10, y = 1:10)
  p <- ggplot(df) +
    geom_col(aes(x = x, y = y), width = 0.8, fill = "red")
  col_layer <- as.list(p$layers[[1]])
  stateless_names <- setdiff(names(col_layer), c("computed_geom_params", "computed_stat_params", "computed_mapping"))
  invisible(ggplotGrob(p))
  expect_identical(as.list(p$layers[[1]])[stateless_names], col_layer[stateless_names])
})

test_that("inherit.aes works", {
  df <- data.frame(x = 1:10, y = 1:10)
  p1 <- ggplot(df, aes(y = y)) +
    geom_col(aes(x = x), inherit.aes = TRUE)
  p2 <- ggplot(df, aes(colour = y)) +
    geom_col(aes(x = x, y = y), inherit.aes = FALSE)
  invisible(ggplotGrob(p1))
  invisible(ggplotGrob(p2))
  expect_identical(p1$layers[[1]]$computed_mapping, p2$layers[[1]]$computed_mapping)
})

test_that("retransform works on computed aesthetics in `map_statistic`", {
  df <- data.frame(x = rep(c(1,2), c(9, 25)))
  p <- ggplot(df, aes(x)) + geom_bar() + scale_y_sqrt()
  expect_equal(layer_data(p)$y, c(3, 5))

  # To double check: should be original values when `retransform = FALSE`
  parent <- p$layers[[1]]$stat
  p$layers[[1]]$stat <- ggproto(NULL, parent, retransform = FALSE)
  expect_equal(layer_data(p)$y, c(9, 25))
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

# Data extraction ---------------------------------------------------------

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

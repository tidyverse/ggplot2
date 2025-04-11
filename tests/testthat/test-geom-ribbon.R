test_that("geom_ribbon() checks the aesthetics", {
  huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
  p <- ggplot(huron) +
    geom_ribbon(aes(year, ymin = level - 5, ymax = level + 5), orientation = "y")
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(huron) +
    geom_ribbon(aes(y = year, xmin = level - 5, xmax = level + 5), orientation = "x")
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(huron) +
    geom_ribbon(aes(year, ymin = level - 5, ymax = level + 5, linewidth = year))
  expect_snapshot_error(ggplotGrob(p))

  expect_snapshot_error(geom_ribbon(aes(year, ymin = level - 5, ymax = level + 5), outline.type = "test"))
})

test_that("NAs are dropped from the data", {
  df <- data_frame(x = 1:5, y = c(1, 1, NA, 1, 1))

  p <- ggplot(df, aes(x))+
    geom_ribbon(aes(ymin = y - 1, ymax = y + 1))
  p <- ggplot_build(p)

  expect_equal(get_layer_data(p)$ymin, c(0, 0, NA, 0, 0))
  expect_snapshot_warning(
    grob <- get_layer_grob(p)[[1]]
  )
  # We expect the ribbon to be broken up into 2 parts
  expect_length(grob$children, 2)
})

test_that("geom_ribbon works in both directions", {
  dat <- data_frame(x = seq_len(5),
                    ymin = c(1, 2, 1.5, 1.8, 1),
                    ymax = c(4, 6, 5, 4.5, 5.2))

  p <- ggplot(dat, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon()
  x <- get_layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(dat, aes(y = x, xmin = ymin, xmax = ymax)) + geom_ribbon()
  y <- get_layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])
})

test_that("outline.type option works", {
  df <- data_frame(x = 1:4, y = c(1, 1, 1, 1))

  p <- ggplot(df, aes(x, ymin = -y, ymax = y))

  g_ribbon_default <- get_layer_grob(p + geom_ribbon())[[1]]
  g_ribbon_upper   <- get_layer_grob(p + geom_ribbon(outline.type = "upper"))[[1]]
  g_ribbon_lower   <- get_layer_grob(p + geom_ribbon(outline.type = "lower"))[[1]]
  g_ribbon_full    <- get_layer_grob(p + geom_ribbon(outline.type = "full"))[[1]]
  g_area_default   <- get_layer_grob(ggplot(df, aes(x, y)) + geom_area(stat = "identity"))[[1]]

  # default
  expect_s3_class(g_ribbon_default$children[[1]]$children[[1]], "polygon")
  expect_s3_class(g_ribbon_default$children[[1]]$children[[2]], "polyline")
  expect_equal(g_ribbon_default$children[[1]]$children[[2]]$id, rep(c(1L, 2L), each = 4))

  # upper
  expect_s3_class(g_ribbon_upper$children[[1]]$children[[1]], "polygon")
  expect_s3_class(g_ribbon_upper$children[[1]]$children[[2]], "polyline")
  expect_equal(g_ribbon_upper$children[[1]]$children[[2]]$id, rep(1L, each = 4))

  # lower
  expect_s3_class(g_ribbon_lower$children[[1]]$children[[1]], "polygon")
  expect_s3_class(g_ribbon_lower$children[[1]]$children[[2]], "polyline")
  expect_equal(g_ribbon_lower$children[[1]]$children[[2]]$id, rep(2L, each = 4))

  # full
  expect_s3_class(g_ribbon_full$children[[1]], "polygon")

  # geom_area()'s default is upper
  expect_s3_class(g_area_default$children[[1]]$children[[1]], "polygon")
  expect_s3_class(g_area_default$children[[1]]$children[[2]], "polyline")
  expect_equal(g_area_default$children[[1]]$children[[2]]$id, rep(1L, each = 4))
})

test_that("ribbons can have gradients", {
  skip_if_not(
    check_device("gradients", action = "test"),
    "graphics device does not support gradients."
  )

  df <- data.frame(x = 1:2, ymin = c(-1:-2), ymax = 1:2)
  p <- ggplot(df, aes(x, ymin = ymin, ymax = ymax, fill = x)) +
    geom_ribbon(outline.type = "full") +
    scale_fill_gradientn(colours = c("red", "blue"))
  fill <- get_layer_grob(p)[[1]]$children[[1]]$gp$fill

  expect_s3_class(fill, "GridLinearGradient")
  expect_equal(fill$colours, alpha(c("red", "blue"), NA))
})

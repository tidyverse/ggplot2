test_that("NAs are not dropped from the data", {
  df <- data_frame(x = 1:5, y = c(1, 1, NA, 1, 1))

  p <- ggplot(df, aes(x))+
    geom_ribbon(aes(ymin = y - 1, ymax = y + 1))

  expect_equal(layer_data(p)$ymin, c(0, 0, NA, 0, 0))
})

test_that("geom_ribbon works in both directions", {
  dat <- data_frame(x = seq_len(5),
                    ymin = c(1, 2, 1.5, 1.8, 1),
                    ymax = c(4, 6, 5, 4.5, 5.2))

  p <- ggplot(dat, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon()
  x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(dat, aes(y = x, xmin = ymin, xmax = ymax)) + geom_ribbon()
  y <- layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])
})

test_that("outline.type option works", {
  df <- data_frame(x = 1:4, y = c(1, 1, 1, 1))

  p <- ggplot(df, aes(x, ymin = -y, ymax = y))

  g_ribbon_default <- layer_grob(p + geom_ribbon())[[1]]
  g_ribbon_upper   <- layer_grob(p + geom_ribbon(outline.type = "upper"))[[1]]
  g_ribbon_lower   <- layer_grob(p + geom_ribbon(outline.type = "lower"))[[1]]
  g_ribbon_full    <- layer_grob(p + geom_ribbon(outline.type = "full"))[[1]]
  g_area_default   <- layer_grob(ggplot(df, aes(x, y)) + geom_area())[[1]]

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

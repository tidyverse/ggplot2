context("scale_discrete")

# Missing values ----------------------------------------------------------

df <- tibble::tibble(
  x1 = c("a", "b", NA),
  x2 = factor(x1),
  x3 = addNA(x2),

  y = 1:3
)

test_that("NAs are translated/preserved for position scales", {
  p1a <- ggplot(df, aes(x1, y)) + geom_point()
  p2a <- ggplot(df, aes(x2, y)) + geom_point()
  p3a <- ggplot(df, aes(x3, y)) + geom_point()

  expect_equal(layer_data(p1a)$x, new_mapped_discrete(c(1, 2, 3)))
  expect_equal(layer_data(p2a)$x, new_mapped_discrete(c(1, 2, 3)))
  expect_equal(layer_data(p3a)$x, new_mapped_discrete(c(1, 2, 3)))

  rm_na_x <- scale_x_discrete(na.translate = FALSE)
  p1b <- p1a + rm_na_x
  p2b <- p2a + rm_na_x
  p3b <- p3a + rm_na_x

  expect_equal(layer_data(p1b)$x, new_mapped_discrete(c(1, 2, NA)))
  expect_equal(layer_data(p2b)$x, new_mapped_discrete(c(1, 2, NA)))
  expect_equal(layer_data(p3b)$x, new_mapped_discrete(c(1, 2, NA)))
})

test_that("NAs are translated/preserved for non-position scales", {
  p1a <- ggplot(df, aes(y, y, colour = x1)) + geom_point()
  p2a <- ggplot(df, aes(y, y, colour = x2)) + geom_point()
  p3a <- ggplot(df, aes(y, y, colour = x3)) + geom_point()
  expect_equal(layer_data(p1a)$colour, c("#F8766D", "#00BFC4", "grey50"))
  expect_equal(layer_data(p2a)$colour, c("#F8766D", "#00BFC4", "grey50"))
  expect_equal(layer_data(p3a)$colour, c("#F8766D", "#00BFC4", "grey50"))

  rm_na_colour <- scale_colour_discrete(na.translate = FALSE)
  p1b <- p1a + rm_na_colour
  p2b <- p2a + rm_na_colour
  p3b <- p3a + rm_na_colour
  expect_equal(layer_data(p1b)$colour, c("#F8766D", "#00BFC4", NA))
  expect_equal(layer_data(p2b)$colour, c("#F8766D", "#00BFC4", NA))
  expect_equal(layer_data(p3b)$colour, c("#F8766D", "#00BFC4", NA))
})

# Ranges ------------------------------------------------------------------

test_that("discrete ranges also encompass continuous values", {
  df <- data_frame(x1 = c("a", "b", "c"), x2 = c(0, 2, 4), y = 1:3)

  base <- ggplot(df, aes(y = y)) + scale_x_discrete()

  x_range <- function(x) {
    layer_scales(x)$x$dimension()
  }

  expect_equal(x_range(base + geom_point(aes(x1))), c(1, 3))
  expect_equal(x_range(base + geom_point(aes(x2))), c(0, 4))
  expect_equal(x_range(base + geom_point(aes(x1)) + geom_point(aes(x2))), c(0, 4))
})

test_that("discrete ranges have limits even when all values are continuous", {
  scale <- scale_x_discrete()
  scale$train(1:3)
  expect_identical(scale$get_limits(), integer())
})

test_that("discrete scale shrinks to range when setting limits", {
  df <- data_frame(x = letters[1:10], y = 1:10)
  p <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_discrete(limits = c("a", "b"))

  expect_equal(layer_scales(p)$x$dimension(c(0, 1)), c(0, 3))
})

test_that("discrete position scales can accept functional limits", {
  scale <- scale_x_discrete(limits = rev)
  scale$train(c("a", "b", "c"))
  expect_identical(scale$get_limits(), c("c", "b", "a"))
})

test_that("discrete non-position scales can accept functional limits", {
  scale <- scale_colour_discrete(limits = rev)
  scale$train(c("a", "b", "c"))
  expect_identical(scale$get_limits(), c("c", "b", "a"))
})

test_that("discrete scale defaults can be set globally", {
  df <- data_frame(
    x = 1:4, y = 1:4,
    two = c("a", "b", "a", "b"),
    four = c("a", "b", "c", "d")
  )

  withr::with_options(
    list(ggplot2.discrete.fill = c("#FFFFFF", "#000000"),
         ggplot2.discrete.colour = c("#FFFFFF", "#000000")), {
      # nlevels == ncodes
      two <- ggplot(df, aes(x, y, colour = two, fill = two)) + geom_point()
      expect_equal(layer_data(two)$colour, rep(c("#FFFFFF", "#000000"), 2))
      expect_equal(layer_data(two)$fill, rep(c("#FFFFFF", "#000000"), 2))

      # nlevels > ncodes (so should fallback to scale_fill_hue())
      four_default <- ggplot(df, aes(x, y, colour = four, fill = four)) +
        geom_point()
      four_hue <- four_default + scale_fill_hue()
      expect_equal(layer_data(four_default)$colour, layer_data(four_hue)$colour)
    }
  )

  withr::with_options(
    list(
      ggplot2.discrete.fill = list(
        c("#FFFFFF", "#000000"),
        c("#FF0000", "#00FF00", "#0000FF", "#FF00FF")
      ),
      ggplot2.discrete.colour = list(
        c("#FFFFFF", "#000000"),
        c("#FF0000", "#00FF00", "#0000FF", "#FF00FF")
      )
    ), {
      # nlevels == 2
      two <- ggplot(df, aes(x, y, colour = two, fill = two)) + geom_point()
      expect_equal(layer_data(two)$colour, rep(c("#FFFFFF", "#000000"), 2))
      expect_equal(layer_data(two)$fill, rep(c("#FFFFFF", "#000000"), 2))

      # nlevels == 4
      four <- ggplot(df, aes(x, y, colour = four, fill = four)) + geom_point()
      expect_equal(layer_data(four)$colour, c("#FF0000", "#00FF00", "#0000FF", "#FF00FF"))
      expect_equal(layer_data(four)$fill, c("#FF0000", "#00FF00", "#0000FF", "#FF00FF"))
    }
  )
})

test_that("Scale is checked in default colour scale", {
  # Check scale type
  expect_error(scale_colour_discrete(type = scale_colour_gradient))
  expect_error(scale_fill_discrete(type = scale_fill_gradient))

  # Check aesthetic
  expect_error(scale_colour_discrete(type = scale_fill_hue))
  expect_error(scale_fill_discrete(type = scale_colour_hue))
})

# mapped_discrete ---------------------------------------------------------

test_that("mapped_discrete vectors behaves as predicted", {
  expect_null(new_mapped_discrete(NULL))
  expect_s3_class(new_mapped_discrete(c(0, 3.5)), "mapped_discrete")
  expect_s3_class(new_mapped_discrete(seq_len(4)), "mapped_discrete")
  expect_error(new_mapped_discrete(letters))

  x <- new_mapped_discrete(1:10)
  expect_s3_class(x[2:4], "mapped_discrete")
  expect_s3_class(c(x, x), "mapped_discrete")
  x[5:7] <- new_mapped_discrete(seq_len(3))
  expect_s3_class(x, "mapped_discrete")
})

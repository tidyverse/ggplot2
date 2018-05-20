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

  expect_equal(layer_data(p1a)$x, c(1, 2, 3))
  expect_equal(layer_data(p2a)$x, c(1, 2, 3))
  expect_equal(layer_data(p3a)$x, c(1, 2, 3))

  rm_na_x <- scale_x_discrete(na.translate = FALSE)
  p1b <- p1a + rm_na_x
  p2b <- p2a + rm_na_x
  p3b <- p3a + rm_na_x

  expect_equal(layer_data(p1b)$x, c(1, 2, NA))
  expect_equal(layer_data(p2b)$x, c(1, 2, NA))
  expect_equal(layer_data(p3b)$x, c(1, 2, NA))
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
  df <- data.frame(x1 = c("a", "b", "c"), x2 = c(0, 2, 4), y = 1:3)

  base <- ggplot(df, aes(y = y)) + scale_x_discrete()

  x_range <- function(x) {
    layer_scales(x)$x$dimension()
  }

  expect_equal(x_range(base + geom_point(aes(x1))), c(1, 3))
  expect_equal(x_range(base + geom_point(aes(x2))), c(0, 4))
  expect_equal(x_range(base + geom_point(aes(x1)) + geom_point(aes(x2))), c(0, 4))
})

test_that("discrete scale shrinks to range when setting limits", {
  df <- data.frame(x = letters[1:10], y = 1:10)
  p <- ggplot(df, aes(x, y)) + geom_point() +
    scale_x_discrete(limits = c("a", "b"))

  expect_equal(layer_scales(p)$x$dimension(c(0, 1)), c(0, 3))
})

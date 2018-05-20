# Test the complete path from plot specification to rendered data
context("Plot building")

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("there is one data frame for each layer", {
  nlayers <- function(x) length(ggplot_build(x)$data)

  l1 <- ggplot(df, aes(x, y)) + geom_point()
  l2 <- ggplot(df, aes(x, y)) + geom_point() + geom_line()
  l3 <- ggplot(df, aes(x, y)) + geom_point() + geom_line() + geom_point()

  expect_equal(nlayers(l1), 1)
  expect_equal(nlayers(l2), 2)
  expect_equal(nlayers(l3), 3)
})

test_that("position aesthetics are coerced to correct type", {
  l1 <- ggplot(df, aes(x, y)) + geom_point()
  d1 <- layer_data(l1, 1)

  expect_is(d1$x, "numeric")
  expect_is(d1$y, "numeric")

  l2 <- ggplot(df, aes(x, z)) + geom_point() + scale_x_discrete()
  d2 <- layer_data(l2, 1)

  expect_is(d2$x, "integer")
  expect_is(d2$y, "integer")
})

test_that("non-position aesthetics are mapped", {
  l1 <- ggplot(df, aes(x, y, fill = z, colour = z, shape = z)) +
    geom_point()

  expect_named(
    layer_data(l1, 1),
    c(
      "x", "y", "fill", "group", "colour", "shape", "size", "PANEL",
      "alpha", "stroke"
    ),
    ignore.order = TRUE
  )

  l2 <- l1 + scale_colour_manual(values = c("blue", "red", "yellow"))
  d2 <- layer_data(l2, 1)
  expect_equal(d2$colour, c("blue", "red", "yellow"))
})

test_that("strings are not converted to factors", {
  df <- data.frame(x = 1:2, y = 2:1, label = c("alpha", "beta"), stringsAsFactors = FALSE)
  p <- ggplot(df, aes(x, y)) +
    geom_text(aes(label = label), parse = TRUE)

  expect_is(layer_data(p)$label, "character")
})

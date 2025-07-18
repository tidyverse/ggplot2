# Test the complete path from plot specification to rendered data
df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("there is one data frame for each layer", {
  nlayers <- function(x) length(ggplot_build(x)@data)

  l1 <- ggplot(df, aes(x, y)) + geom_point()
  l2 <- ggplot(df, aes(x, y)) + geom_point() + geom_line()
  l3 <- ggplot(df, aes(x, y)) + geom_point() + geom_line() + geom_point()

  expect_equal(nlayers(l1), 1)
  expect_equal(nlayers(l2), 2)
  expect_equal(nlayers(l3), 3)
})

test_that("position aesthetics are coerced to correct type", {
  l1 <- ggplot(df, aes(x, y)) + geom_point()
  d1 <- get_layer_data(l1, 1)

  expect_type(d1$x, "double")
  expect_type(d1$y, "double")

  l2 <- ggplot(df, aes(x, z)) + geom_point() + scale_x_discrete()
  d2 <- get_layer_data(l2, 1)

  expect_s3_class(d2$x, "mapped_discrete")
  expect_s3_class(d2$y, "mapped_discrete")
})

test_that("non-position aesthetics are mapped", {
  l1 <- ggplot(df, aes(x, y, fill = z, colour = z, shape = z)) +
    geom_point()

  expect_named(
    get_layer_data(l1, 1),
    c(
      "x", "y", "fill", "group", "colour", "shape", "size", "PANEL",
      "alpha", "stroke"
    ),
    ignore.order = TRUE
  )

  l2 <- l1 + scale_colour_manual(values = c("blue", "red", "yellow"))
  d2 <- get_layer_data(l2, 1)
  expect_equal(d2$colour, c("blue", "red", "yellow"))
})

test_that("strings are not converted to factors", {
  df <- data_frame(x = 1:2, y = 2:1, label = c("alpha", "beta"))
  p <- ggplot(df, aes(x, y)) +
    geom_text(aes(label = label), parse = TRUE)

  expect_type(get_layer_data(p)$label, "character")
})

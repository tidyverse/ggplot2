test_that("can control whether to preserve total or individual width", {
  df <- data_frame(x = c("a", "b", "b"), y = c("a", "a", "b"))

  p_total <- ggplot(df, aes(x, fill = y)) +
    geom_bar(position = position_dodge(preserve = "total"), width = 1)
  p_single <- ggplot(df, aes(x, fill = y)) +
    geom_bar(position = position_dodge(preserve = "single"), width = 1)

  expect_equal(get_layer_data(p_total)$x, new_mapped_discrete(c(1, 1.75, 2.25)))
  expect_equal(get_layer_data(p_single)$x, new_mapped_discrete(c(0.75, 1.75, 2.25)))
})

test_that("position_dodge() can dodge points vertically", {

  df <- data.frame(x = c(1, 2, 3, 4), y = c("a", "a", "b", "b"))

  horizontal <- ggplot(df, aes(y, x, group = seq_along(x))) +
    geom_point(position = position_dodge(width = 1, orientation = "x"))
  vertical <- ggplot(df, aes(x, y, group = seq_along(x))) +
    geom_point(position = position_dodge(width = 1, orientation = "y"))

  expect_equal(layer_data(horizontal)$x, c(0.75, 1.25, 1.75, 2.25), ignore_attr = "class")
  expect_equal(layer_data(vertical)$y,   c(0.75, 1.25, 1.75, 2.25), ignore_attr = "class")

})

test_that("position_dodge() can reverse the dodge order", {

  df <- data.frame(x = c(1, 2, 2, 3, 3), group = c("A", "A", "B", "B", "C"))

  # Use label as easy to track identifier
  p <- ggplot(df, aes(x, y = 1, fill = group, label = group))

  ld <- get_layer_data(p + geom_col(position = position_dodge(reverse = TRUE)))
  expect_equal(ld$label[order(ld$x)], c("A", "B", "A", "C", "B"))

  ld <- get_layer_data(p + geom_col(position = position_dodge(reverse = FALSE)))
  expect_equal(ld$label[order(ld$x)], c("A", "A", "B", "B", "C"))
})

test_that("position_dodge warns about missing required aesthetics", {

  # Bit of a contrived geom to not have a required 'x' aesthetic
  GeomDummy <- ggproto(NULL, GeomPoint, required_aes = NULL, optional_aes = "x")

  p <- ggplot(mtcars, aes(cyl, disp, colour = factor(vs))) +
    layer(
      geom = GeomDummy,
      stat = "identity",
      position = position_dodge(width = 0.5),
      mapping = aes(x = NULL)
    )

  expect_snapshot(ggplot_build(p), error = TRUE)
})

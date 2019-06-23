context("coord_trans")

test_that("warnings are generated when cord_trans() results in new infinite values", {
  p  <- ggplot(head(diamonds, 20)) +
    geom_bar(aes(x = cut)) +
    coord_trans(y = "log10")

  p2 <- ggplot(data_frame(a = c(1, 2, 0), b = c(10, 6, 4)), aes(a, b)) +
    geom_point() +
    coord_trans(x = "log")

  expect_warning(ggplot_gtable(ggplot_build(p)), "Transformation introduced infinite values in y-axis")
  expect_warning(ggplot_gtable(ggplot_build(p2)), "Transformation introduced infinite values in x-axis")
})

test_that("no warnings are generated when original data has Inf values, but no new Inf values created from the transformation", {
  p <- ggplot(data_frame(x = c(-Inf, 2, 0), y = c(Inf, 6, 4)), aes(x, y)) +
    geom_point() +
    coord_trans(x = 'identity')

  expect_silent(benchplot(p))
})

test_that("coord_trans() expands axes the identically coord_cartesian()", {
  p <- ggplot(mpg, aes(class, hwy)) + geom_point()
  built_cartesian <- ggplot_build(p + coord_cartesian())
  built_trans <- ggplot_build(p + coord_trans())

  cartesian_params <- built_cartesian$layout$panel_params[[1]]
  trans_params <- built_trans$layout$panel_params[[1]]

  expect_identical(cartesian_params$x.range, trans_params$x.range)
  expect_identical(cartesian_params$y.range, trans_params$y.range)
})

test_that("coord_trans(expand = FALSE) expands axes the identically coord_cartesian(expand = FALSE)", {
  p <- ggplot(mpg, aes(class, hwy)) + geom_point()
  built_cartesian <- ggplot_build(p + coord_cartesian(expand = FALSE))
  built_trans <- ggplot_build(p + coord_trans(expand = FALSE))

  cartesian_params <- built_cartesian$layout$panel_params[[1]]
  trans_params <- built_trans$layout$panel_params[[1]]

  expect_identical(cartesian_params$x.range, trans_params$x.range)
  expect_identical(cartesian_params$y.range, trans_params$y.range)
})


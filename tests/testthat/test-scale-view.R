test_that("ViewScales can make fixed copies", {

  p1 <- ggplot(mpg, aes(drv, displ)) +
    geom_boxplot() +
    annotate("point", x = 5, y = 10) +
    scale_x_discrete(labels = c("four-wheel", "forward", "reverse"))

  b1 <- ggplot_build(p1)@layout$panel_params[[1]]

  # We build a second plot with the first plot's scales
  p2 <- ggplot(mpg, aes(drv, cyl)) +
    geom_violin() +
    annotate("point", x = 15, y = 100) +
    b1$x$make_fixed_copy() +
    b1$y$make_fixed_copy()
  b2 <- ggplot_build(p2)

  # Breaks and labels should respect p1's limits
  x <- get_guide_data(b2, "x")
  expect_equal(x$x, 0.6:2.6 / diff(b1$x.range))
  expect_equal(x$.label, c("four-wheel", "forward", "reverse"))

  y <- get_guide_data(b2, "y")
  expect_equal(y$y, rescale(seq(2.5, 10, by = 2.5), from = b1$y.range))
})

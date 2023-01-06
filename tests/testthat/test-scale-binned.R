test_that("binned scales only support continuous data", {
  p <- ggplot(mtcars) + geom_bar(aes(as.character(gear))) + scale_x_binned()
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, colour = as.character(gear))) + scale_color_binned()
  expect_snapshot_error(ggplot_build(p))
})

test_that("binned scales limits can expand to fit breaks", {
  # See also #5095

  scale <- scale_x_binned(right = FALSE, show.limits = TRUE)
  scale$train(c(14, 29))

  limits <- scale$get_limits()
  breaks <- scale$get_breaks()
  new_limits <- scale$get_limits()

  # Positive control
  expect_equal(limits,     c(14, 29))
  # Test case, should have been updated in break calculation
  expect_equal(new_limits, c(14, 30))

  # Negative control
  # Now, new limits should not be updated because limits were given instead
  # of computed
  scale <- scale_x_binned(right = FALSE, show.limits = TRUE,
                          limits = c(14, 29))
  limits <- scale$get_limits()
  breaks <- scale$get_breaks()
  new_limits <- scale$get_limits()

  expect_equal(limits, new_limits)
})

test_that("binned limits should not compute out-of-bounds breaks", {
  scale <- scale_x_binned(n.breaks = 10)
  scale$train(c(1, 9))

  limits <- scale$get_limits()
  breaks <- scale$get_breaks()
  expect_length(breaks, 7) # Not the requested 10 due to oob discarding
  expect_true(all(
    breaks > limits[1] & breaks < limits[2]
  ))
})

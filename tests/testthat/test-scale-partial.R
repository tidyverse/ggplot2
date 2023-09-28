
test_that("partial scales can be updated", {

  # Partial scales
  xscale1 <- scale_x(name = "foobar", limits = c(0, 10))
  xscale2 <- scale_x(limits = c(0, 1))

  plot1 <- ggplot() + xscale1

  expect_s3_class(
    plot1$scales$get_scales("x"),
    "ScalePartial"
  )
  expect_equal(
    plot1$scales$get_scales("x")$params,
    list(name = "foobar", limits = c(0, 10))
  )

  # Update scale
  plot2 <- plot1 + xscale2

  # Check update was successful
  expect_equal(
    plot2$scales$get_scales("x")$params,
    list(limits = c(0, 1), name = "foobar")
  )
  # Check state of plot1 hasn't changed
  expect_equal(
    plot1$scales$get_scales("x")$params,
    list(name = "foobar", limits = c(0, 10))
  )
  # Check state of xscale1 is unchanged
  expect_equal(xscale1$params, list(name = "foobar", limits = c(0, 10)))
  # Check state of xscale2 is unchanged
  expect_equal(xscale2$params, list(limits = c(0, 1)))

  # Add default scales
  plot2$scales$add_missing(c("x", "y"), env = current_env())

  full <- plot2$scales$get_scales("x")
  expect_s3_class(full, "ScaleContinuousPosition")
  expect_equal(full$name, "foobar")
  expect_equal(full$limits, c(0, 1))
})

test_that("partial scale input is checked", {

  expect_error(scale_x(10), "must be named")
  expect_snapshot_error(
    scale_x(limits = c(0, 10), limits = c(1, 2))
  )

  # Check for nonsense arguments
  p <- ggplot() + scale_x(foo = "bar", limits = c(0, 1))

  expect_warning(
    p$scales$add_missing(c("x", "y"), env = current_env()),
    "Ignoring unknown scale parameter"
  )

  # Check incompatible arguments
  p <- ggplot() + scale_x(breaks = c(1, 2), labels = c("A", "B", "C"))
  expect_error(
    p$scales$add_missing(c("x", "y"), env = current_env()),
    "must have the same length"
  )

  expect_error(
    ggplot() + scale_x(limits = c(0, 10)) + scale_x_discrete(),
    "Continuous limits supplied to discrete scale"
  )
  expect_error(
    ggplot() + scale_x(limits = c("A", "B")) + scale_x_continuous(),
    "Discrete limits supplied to continuous scale"
  )

})

test_that("limits are updated with transformations", {

  p1 <- ggplot() + scale_x_log10(limits = c(1, 100))

  expect_equal(p1$scales$get_scales("x")$limits, c(0, 2))

  p2 <- p1 + scale_x(trans = "sqrt")

  expect_equal(p2$scales$get_scales("x")$limits, c(1, 10))

  p3 <- p2 + scale_x(trans = "identity")

  expect_equal(p3$scales$get_scales("x")$limits, c(1, 100))

  p1 <- ggplot() + scale_x(limits = c(1, 100))

  p2 <- p1 + scale_x_log10()

  expect_equal(p2$scales$get_scales("x")$limits, c(0, 2))

})

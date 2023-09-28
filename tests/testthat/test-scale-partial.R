
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

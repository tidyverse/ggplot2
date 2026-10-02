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

test_that("binned scales can use limits and transformations simultaneously (#6144)", {
  s <- scale_x_binned(
    limits = function(x) x + 1,
    transform = transform_log10()
  )
  s$train(c(0, 1)) # c(1, 10) in untransformed space
  out <- s$get_limits()
  expect_equal(s$get_limits(), log10(c(2, 11)))
})

test_that("binned scales can use NAs in limits", {
  scale <- scale_x_binned(limits = c(NA, 10))
  scale$train(c(-20, 20))
  expect_equal(scale$get_limits(), c(-20, 10))
  scale <- scale_x_binned(limits = c(-10, NA))
  scale$train(c(-20, 20))
  expect_equal(scale$get_limits(), c(-10, 20))
})

test_that("binned scales can calculate breaks with reverse transformation", {
  scale <- scale_x_binned(transform = "reverse")
  scale$train(c(1, 9))
  expect_equal(scale$get_breaks(), 8:2)
})

test_that('binned scales can calculate breaks on dates', {

  data <- seq(as.Date("2000-01-01"), as.Date("2020-01-01"), length.out = 100)

  scale <- scale_x_binned(transform = "date")
  scale$train(scale$transform(data))
  breaks <- scale$get_transformation()$inverse(scale$get_breaks())

  expect_s3_class(breaks, "Date")
  expect_equal(
    unname(breaks),
    as.Date(paste0(seq(2002, 2018, by = 2), "-01-01"))
  )
})

test_that('binned scales can calculate breaks on date-times', {
  data <- seq(
    strptime("2000-01-01", "%Y-%m-%d"),
    strptime("2020-01-01", "%Y-%m-%d"),
    length.out = 100
  )

  scale <- scale_x_binned(transform = "time")
  scale$train(scale$transform(data))
  breaks <- scale$get_transformation()$inverse(scale$get_breaks())

  expect_s3_class(breaks, "POSIXct")
  expect_equal(
    unname(unclass(breaks)),
    unclass(as.POSIXct(strptime(
      paste0(seq(2002, 2018, by = 2), "-01-01"),
      "%Y-%m-%d"
    )))
  )
})

test_that("binned scales can calculate breaks for zero-width data", {
  scale <- scale_x_binned()
  scale$train(c(1, 1))
  expect_equal(scale$get_breaks(), c(0.95, 1.05))
})

test_that("warnings are generated when coord_trans() results in new infinite values", {
  p  <- ggplot(head(diamonds, 20)) +
    geom_bar(aes(x = cut)) +
    coord_trans(y = "log10")

  p2 <- ggplot(data_frame(a = c(1, 2, 0), b = c(10, 6, 4)), aes(a, b)) +
    geom_point() +
    coord_trans(x = "log")

  # TODO: These multiple warnings should be summarized nicely. Until this gets
  #       fixed, this test ignores all the following errors than the first one.
  suppressWarnings({
    expect_snapshot_warning(ggplot_gtable(ggplot_build(p)))
    expect_snapshot_warning(ggplot_gtable(ggplot_build(p2)))
  })
})

test_that("no warnings are generated when original data has Inf values, but no new Inf values created from the transformation", {
  p <- ggplot(data_frame(x = c(-Inf, 2, 0), y = c(Inf, 6, 4)), aes(x, y)) +
    geom_point() +
    coord_trans(x = 'identity')

  expect_silent(benchplot(p))
})

test_that("coord_trans() expands axes identically to coord_cartesian()", {
  p <- ggplot(mpg, aes(class, hwy)) + geom_point()
  built_cartesian <- ggplot_build(p + coord_cartesian())
  built_trans <- ggplot_build(p + coord_trans())

  cartesian_params <- built_cartesian$layout$panel_params[[1]]
  trans_params <- built_trans$layout$panel_params[[1]]

  expect_identical(cartesian_params$x.range, trans_params$x.range)
  expect_identical(cartesian_params$y.range, trans_params$y.range)
})

test_that("coord_trans(expand = FALSE) expands axes identically to coord_cartesian(expand = FALSE)", {
  p <- ggplot(mpg, aes(class, hwy)) + geom_point()
  built_cartesian <- ggplot_build(p + coord_cartesian(expand = FALSE))
  built_trans <- ggplot_build(p + coord_trans(expand = FALSE))

  cartesian_params <- built_cartesian$layout$panel_params[[1]]
  trans_params <- built_trans$layout$panel_params[[1]]

  expect_identical(cartesian_params$x.range, trans_params$x.range)
  expect_identical(cartesian_params$y.range, trans_params$y.range)
})

test_that("coord_trans(y = 'log10') expands the x axis identically to scale_y_log10()", {
  p <- ggplot(mpg, aes(class, hwy)) + geom_point()
  built_cartesian <- ggplot_build(p + scale_y_log10())
  built_trans <- ggplot_build(p + coord_trans(y = "log10"))

  cartesian_params <- built_cartesian$layout$panel_params[[1]]
  trans_params <- built_trans$layout$panel_params[[1]]

  expect_identical(cartesian_params$y.range, trans_params$y.range)
})

test_that("coord_trans() expands axes outside the domain of the axis trans", {
  # transform_sqrt() has a lower limit of 0
  df <- data_frame(x = 1, y = c(0, 1, 2))
  p <- ggplot(df, aes(x, y)) + geom_point()
  built_cartesian <- ggplot_build(p + scale_y_sqrt())
  built_trans <- ggplot_build(p + coord_trans(y = "sqrt"))

  cartesian_params <- built_cartesian$layout$panel_params[[1]]
  trans_params <- built_trans$layout$panel_params[[1]]

  expect_identical(cartesian_params$y.range, trans_params$y.range)
})

test_that("coord_trans() works with the reverse transformation", {
  df <- data_frame(x = c("1-one", "2-two", "3-three"), y = c(20, 30, 40))

  p <- ggplot(df, aes(x, y)) + geom_point()
  built_cartesian <- ggplot_build(p + scale_y_reverse())
  built_trans <- ggplot_build(p + coord_trans(y = "reverse"))

  cartesian_params <- built_cartesian$layout$panel_params[[1]]
  trans_params <- built_trans$layout$panel_params[[1]]

  expect_identical(cartesian_params$y.range, trans_params$y.range)
})

test_that("coord_trans() can reverse discrete axes", {
  df <- data_frame(x = c("1-one", "2-two", "3-three"), y = c(20, 30, 40))

  p <- ggplot(df, aes(x, y)) + geom_point()
  built_cartesian <- ggplot_build(p)
  built_trans <- ggplot_build(p + coord_trans(x = "reverse"))

  cartesian_params <- built_cartesian$layout$panel_params[[1]]
  trans_params <- built_trans$layout$panel_params[[1]]

  expect_identical(cartesian_params$x.range, -rev(trans_params$x.range))
})

test_that("basic coord_trans() plot displays both continuous and discrete axes", {
  expect_doppelganger(
    "basic coord_trans() plot",
    ggplot(mpg, aes(class, hwy)) +
      geom_point() +
      coord_trans(y = "log10")
  )
})

test_that("second axes display in coord_trans()", {
  expect_doppelganger(
    "sec_axis with coord_trans()",
    ggplot(mpg, aes(cty, hwy)) +
      geom_point() +
      scale_y_continuous(
        sec.axis = sec_axis(
          transform = ~log2(.),
          breaks = c(3.5, 4, 4.5, 5, 5.5),
          name = "log2(hwy)"
        ),
        breaks = 2^c(3.5, 4, 4.5, 5, 5.5)
      ) +
      scale_x_continuous(sec.axis = dup_axis()) +
      coord_trans(y = "log2")
  )
})

test_that("coord_trans() throws error when limits are badly specified", {
  # throws error when limit is a Scale object instead of vector
  expect_snapshot_error(ggplot() + coord_trans(xlim=xlim(1,1)))

  # throws error when limit's length is different than two
  expect_snapshot_error(ggplot() + coord_trans(ylim=1:3))
})

test_that("transformed coords can be reversed", {
  p <- ggplot(data_frame0(x = c(1, 100), y = c(1, 100))) +
    aes(x = x, y = y) +
    geom_point() +
    coord_trans(
      x = "log10", y = "log10",
      xlim = c(0.1, 1000), ylim = c(0.1, 1000), expand = FALSE,
      reverse = "xy"
    )
  grob <- layer_grob(p)[[1]]
  expect_equal(as.numeric(grob$x), c(0.75, 0.25))
  expect_equal(as.numeric(grob$y), c(0.75, 0.25))
})

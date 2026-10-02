test_that("stat_summary(_bin) work with lambda expressions", {
  # note: stat_summary and stat_summary_bin both use
  # make_summary_fun, so this tests both

  dat <- data_frame(
    x = c(1, 1, 2, 2, 3, 3),
    y = c(0, 2, 1, 3, 2, 4)
  )

  p1 <- ggplot(dat, aes(x, y)) +
    stat_summary(fun.data = mean_se)


  # test fun.data
  p2 <- ggplot(dat, aes(x, y)) +
    stat_summary(fun.data = ~ {
      mean <- mean(.x)
      se <- sqrt(stats::var(.x) / length(.x))
      data_frame(y = mean, ymin = mean - se, ymax = mean + se)
    })

  expect_equal(
    get_layer_data(p1),
    get_layer_data(p2)
  )


  # fun, fun.min, fun.max
  p3 <- ggplot(dat, aes(x, y)) +
    stat_summary(
      fun = ~ mean(.x),
      fun.min = ~ mean(.x) - sqrt(stats::var(.x) / length(.x)),
      fun.max = ~ mean(.x) + sqrt(stats::var(.x) / length(.x))
    )

  expect_equal(
    get_layer_data(p1),
    get_layer_data(p3)
  )

})

test_that("stat_summary_bin takes user's `width` argument (#4647)", {
  p <- ggplot(mtcars, aes(mpg, disp)) +
    stat_summary_bin(
      fun.data = mean_se, na.rm = TRUE,
      binwidth = 1, width = 2
    )

  ld <- layer_data(p)
  expect_equal(unique(ld$width), 2)
})

test_that("stat_summary_(2d|hex) work with lambda expressions", {

  dat <- data_frame(
    x = c(0, 0, 0, 0, 1, 1, 1, 1),
    y = c(0, 0, 1, 1, 0, 0, 1, 1),
    z = c(1, 1, 2, 2, 2, 2, 3, 3)
  )


  # stat_summary_2d
  p1 <- ggplot(dat, aes(x, y, z = z)) +
    stat_summary_2d(fun = function(x) mean(x))

  p2 <- ggplot(dat, aes(x, y, z = z)) +
    stat_summary_2d(fun = ~ mean(.x))

  expect_equal(
    get_layer_data(p1),
    get_layer_data(p2)
  )



  # stat_summary_hex
  # this plot is a bit funky, but easy to reason through
  skip_if_not_installed("hexbin")
  p1 <- ggplot(dat, aes(x, y, z = z)) +
    stat_summary_hex(fun = function(x) mean(x))

  p2 <- ggplot(dat, aes(x, y, z = z)) +
    stat_summary_hex(fun = ~ mean(.x))

  expect_equal(
    get_layer_data(p1),
    get_layer_data(p2)
  )

})

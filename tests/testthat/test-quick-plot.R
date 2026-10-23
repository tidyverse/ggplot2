test_that("qplot works with variables in data frame and parent env", {
  df <- data_frame(x = 1:10, a = 1:10)
  y <- 1:10
  b <- 1:10

  lifecycle::expect_deprecated(
    p <- qplot(x, y, data = df)
  )
  expect_s7_class(p, class_ggplot)
  lifecycle::expect_deprecated(
    p <- qplot(x, y, data = df, colour = a)
  )
  expect_s7_class(p, class_ggplot)
  lifecycle::expect_deprecated(
    p <- qplot(x, y, data = df, colour = b)
  )
  expect_s7_class(p, class_ggplot)

  bin <- 1
  lifecycle::expect_deprecated(
    p <- qplot(x, data = df, binwidth = bin)
  )
  expect_s7_class(p, class_ggplot)
})

test_that("qplot works in non-standard environments", {
  lifecycle::expect_deprecated(
    p <- local({
      `-1-` <- 10
      x <- 1:10
      qplot(x, breaks = 0:`-1-`)
    })
  )
  expect_s7_class(p, class_ggplot)
})

test_that("qplot() evaluates constants in the right place", {
  lifecycle::expect_deprecated(
    p <- local({
      foo <- "d"
      qplot(1, 1, colour = I(paste0("re", foo)))
    })
  )
  expect_identical(get_layer_data(p)$colour, I("red"))
})

test_that("qplot() evaluates layers in package environment", {
  geom_line <- function(...) {
    stop("!!!")
  }

  lifecycle::expect_deprecated(
    expect_no_error(p <- qplot(1, 1, geom = "line"))
  )
})

test_that("qplot() only work with character geom", {
  lifecycle::expect_deprecated(
    expect_snapshot_error(qplot(geom = GeomLinerange))
  )
})

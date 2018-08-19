context("sec-axis")

x <- exp(seq(log(0.001), log(1000), length.out = 100))
foo <- data.frame(
  x = x,
  y = x / (1 + x)
)

test_that("dup_axis() works", {
  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_x_continuous(
      name = "Unit A",
      sec.axis = dup_axis()
    )
  scale <- layer_scales(p)$x
  expect_equal(scale$sec_name(), scale$name)
  breaks <- scale$break_info()
  expect_equal(breaks$minor, breaks$sec.minor)
  expect_equal(breaks$major_source, breaks$sec.major_source)
})

test_that("sec_axis() breaks work for log-transformed scales", {
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c(10, 100, 1000)
  )

  # dup_axis()
  p <- ggplot(data = df, aes(x, y)) +
    geom_point() +
    scale_y_log10(sec.axis = dup_axis())

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  expect_equal(breaks$major_source, breaks$sec.major_source)

  # sec_axis() with transform
  p <- ggplot(data = df, aes(x, y)) +
    geom_point() +
    scale_y_log10(sec.axis = sec_axis(~. * 100))

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  expect_equal(breaks$major_source, breaks$sec.major_source - 2)

  # sec_axis() with transform and breaks
  custom_breaks <- c(10, 20, 40, 200, 400, 800)
  p <- ggplot(data = df, aes(x, y)) +
    geom_point() +
    scale_y_log10(breaks = custom_breaks, sec.axis = sec_axis(~. * 100))

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  expect_equal(breaks$major_source, log(custom_breaks, base = 10))
  expect_equal(log_breaks()(df$y) * 100, 10^(breaks$sec.major_source))
})

test_that("custom breaks work", {
  custom_breaks <- c(0.01, 0.1, 1, 10, 100)
  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_x_continuous(
      name = "Unit A",
      sec.axis = sec_axis(
        trans = y ~ .,
        breaks = custom_breaks
      )
    )
  scale <- layer_scales(p)$x
  breaks <- scale$break_info()
  expect_equal(custom_breaks, breaks$sec.major_source)
})

test_that("sec axis works with skewed transform", {
  expect_doppelganger(
    "sec_axis, skewed transform",
    ggplot(foo, aes(x, y)) +
      geom_point() +
      scale_x_continuous(
        name = "Unit A", trans = "log",
        breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
        sec.axis = sec_axis(~. * 100,
          name = "Unit B",
          labels = derive(),
          breaks = derive()
        )
      )
  )
})

test_that("sec axis works with tidy eval", {
  f <- function(df, .x, .y, .z) {
    x <- enquo(.x)
    y <- enquo(.y)
    z <- enquo(.z)

    g <- ggplot(df, aes(x = !!x, y = !!y)) +
      geom_bar(stat = "identity") +
      geom_point(aes(y = !!z)) +
      scale_y_continuous(sec.axis = sec_axis(~. / 10))

    g
  }

  t <- tibble(x = letters, y = seq(10, 260, 10), z = 1:26)

  p <- f(t, x, y, z)

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  expect_equal(breaks$major_source / 10, breaks$sec.major_source)
})

test_that("sec_axis() works for power transformations (monotonicity test doesn't fail)", {
  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_x_sqrt(sec.axis = dup_axis())
  scale <- layer_scales(p)$x
  breaks <- scale$break_info()
  expect_equal(breaks$major, breaks$sec.major, tolerance = .001)

  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_x_sqrt(sec.axis = sec_axis(~. * 100))
  scale <- layer_scales(p)$x
  breaks <- scale$break_info()
  expect_equal(breaks$major, breaks$sec.major, tolerance = .001)
})

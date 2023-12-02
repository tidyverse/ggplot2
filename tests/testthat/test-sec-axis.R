x <- exp(seq(log(0.001), log(1000), length.out = 100))
foo <- data_frame(
  x = x,
  y = x / (1 + x)
)

test_that("sec_axis checks the user input", {
  scale <- scale_x_continuous()
  expect_snapshot_error(set_sec_axis(16, scale))
  expect_silent(set_sec_axis(~ .^2, scale))
  secondary <- ggproto(NULL, AxisSecondary, trans = 1:10)
  expect_snapshot_error(secondary$init(scale))

  p <- ggplot(mtcars) + geom_point(aes(disp, mpg)) + scale_y_continuous(sec.axis = ~sin(.))
  expect_snapshot_error(ggplot_build(p))

  p <- ggplot(mtcars) + geom_point(aes(disp, mpg)) + scale_y_continuous(sec.axis = ~sin(./100))
  expect_silent(ggplot_build(p))
})

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
  expect_equal(breaks$minor_source, breaks$sec.minor_source_user)
  expect_equal(breaks$major_source, breaks$sec.major_source_user)

  # these aren't exactly equal because the sec_axis trans is based on a
  # (default) 1000-point approximation
  expect_true(all(abs(breaks$major_source - round(breaks$sec.major_source) <= 1)))
  expect_true(all(abs(breaks$minor_source - round(breaks$sec.minor_source) <= 1)))
  expect_equal(round(breaks$major, 3), round(breaks$major, 3))
  expect_equal(round(breaks$minor, 3), round(breaks$minor, 3))
})

test_that("sec_axis() works with subtraction", {
  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_y_continuous(
      sec.axis = sec_axis(~1-.)
    )
  scale <- layer_scales(p)$y
  expect_equal(scale$sec_name(), scale$name)
  breaks <- scale$break_info()
  expect_equal(breaks$minor_source, breaks$sec.minor_source_user)
  expect_equal(breaks$major_source, breaks$sec.major_source_user)

  # these aren't exactly equal because the sec_axis trans is based on a
  # (default) 1000-point approximation
  expect_true(all(abs(breaks$major_source - round(breaks$sec.major_source) <= 1)))
  expect_true(all(abs(breaks$minor_source - round(breaks$sec.minor_source) <= 1)))
  expect_equal(round(breaks$major, 3), round(breaks$major, 3))
  expect_equal(round(breaks$minor, 3), round(breaks$minor, 3))
})

test_that("sex axis works with division (#1804)", {
  expect_doppelganger(
    "sec_axis, with division",
    ggplot(mpg, aes(displ, hwy)) +
      geom_point() +
      scale_y_continuous(sec.axis = sec_axis(~ 235 / ., name = "100km / L")) +
      theme_linedraw()
  )
})


test_that("sec_axis() breaks work for log-transformed scales", {
  df <- data_frame(
    x = c("A", "B", "C"),
    y = c(10, 100, 1000)
  )

  # dup_axis()
  p <- ggplot(data = df, aes(x, y)) +
    geom_point() +
    scale_y_log10(sec.axis = dup_axis())

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  # test value
  expect_equal(breaks$major_source, log10(breaks$sec.major_source_user))
  expect_equal(round(breaks$major_source, 2), round(breaks$sec.major_source, 2))

  # test position
  expect_equal(breaks$major, round(breaks$sec.major, 1))

  # sec_axis() with transform
  p <- ggplot(data = df, aes(x, y)) +
    geom_point() +
    scale_y_log10(sec.axis = sec_axis(~ . * 100))

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  # test value
  expect_equal(breaks$major_source, log10(breaks$sec.major_source_user) - 2)
  expect_equal(breaks$major_source, round(breaks$sec.major_source, 2))

  # test position
  expect_equal(breaks$major, round(breaks$sec.major, 1))


  # sec_axis() with transform and breaks
  custom_breaks <- c(10, 20, 40, 200, 400, 800)
  p <- ggplot(data = df, aes(x, y)) +
    geom_point() +
    scale_y_log10(breaks = custom_breaks, sec.axis = sec_axis(~ . * 100))

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  expect_equal(breaks$major_source, log(custom_breaks, base = 10))
  expect_equal(log_breaks()(df$y) * 100, breaks$sec.major_source_user)
})

test_that("custom breaks work", {
  custom_breaks <- c(100, 375, 800)
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
  expect_equal(custom_breaks, breaks$sec.major_source_user)
})

test_that("sec axis works with skewed transform", {
  expect_doppelganger(
    "sec_axis, skewed transform",
    ggplot(foo, aes(x, y)) +
      geom_point() +
      scale_x_continuous(
        name = "Unit A", trans = "log",
        breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
        sec.axis = sec_axis(~ . * 100,
          name = "Unit B",
          labels = derive(),
          breaks = derive()
        )
      ) + theme_linedraw()
  )
})

test_that("sec axis works with tidy eval", {
  # decoy, not used
  a <- 5

  f <- function(df, .x, .y, .z) {
    x <- enquo(.x)
    y <- enquo(.y)
    z <- enquo(.z)
    a <- 10 # scaling of secondary axis

    g <- ggplot(df, aes(x = !!x, y = !!y)) +
      geom_bar(stat = "identity") +
      geom_point(aes(y = !!z)) +
      scale_y_continuous(sec.axis = sec_axis(~ . / a))

    g
  }

  t <- tibble(x = letters, y = seq(10, 260, 10), z = 1:26)

  p <- f(t, x, y, z)

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  # test transform
  expect_equal(breaks$major_source / 10, breaks$sec.major_source_user)
  # test positioning
  expect_equal(round(breaks$major, 2), round(breaks$sec.major, 2))
})

test_that("sec_axis() handles secondary power transformations", {
  set.seed(111)
  df <- data_frame(
    x = rnorm(100),
    y = rnorm(100)
  )
  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    scale_y_continuous(sec.axis = sec_axis(trans = (~ 2^.)))

  scale <- layer_scales(p)$y
  breaks <- scale$break_info()

  expect_equal(round(breaks$major[4:6], 2), round(breaks$sec.major[c(1, 2, 4)], 2))

  expect_doppelganger(
    "sec_axis, sec power transform",
    ggplot() +
      geom_point(aes(x = 1:10, y = rep(5, 10))) +
      scale_x_continuous(sec.axis = sec_axis(~ log10(.))) +
      theme_linedraw()
  )
})

test_that("sec_axis() respects custom transformations", {
  # Custom transform code submitted by DInfanger, Issue #2798
  magnify_trans_log <- function(interval_low = 0.05, interval_high = 1, reducer = 0.05, reducer2 = 8) {
    trans <- Vectorize(function(x, i_low = interval_low, i_high = interval_high, r = reducer, r2 = reducer2) {
      if (is.na(x) || (x >= i_low & x <= i_high)) {
        x
      } else if (x < i_low & !is.na(x)) {
        (log10(x / r) / r2 + i_low)
      } else {
        log10((x - i_high) / r + i_high) / r2
      }
    })

    inv <- Vectorize(function(x, i_low = interval_low, i_high = interval_high, r = reducer, r2 = reducer2) {
      if (is.na(x) || (x >= i_low & x <= i_high)) {
        x
      } else if (x < i_low & !is.na(x)) {
        10^(-(i_low - x) * r2) * r
      } else {
        i_high + 10^(x * r2) * r - i_high * r
      }
    })

    trans_new(name = "customlog", transform = trans, inverse = inv, domain = c(1e-16, Inf))
  }

  # Create data
  x <- seq(-1, 1, length.out = 1000)
  y <- c(x[x < 0] + 1, -x[x > 0] + 1) + 1e-6
  dat <- data_frame(x = c(NA, x), y = c(1, y))

  expect_doppelganger(
    "sec_axis, custom transform",
    ggplot(dat, aes(x = x, y = y)) +
      geom_line(linewidth = 1, na.rm = T) +
      scale_y_continuous(
        trans =
          magnify_trans_log(interval_low = 0.5, interval_high = 1, reducer = 0.5, reducer2 = 8), breaks =
          c(0.001, 0.01, 0.1, 0.5, 0.6, 0.7, 0.8, 0.9, 1), limits =
          c(0.001, 1), sec.axis = sec_axis(
          trans =
            ~ . * (1 / 2), breaks = c(0.001, 0.01, 0.1, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
        )
      ) + theme_linedraw()
  )
})

test_that("sec_axis works with date/time/datetime scales", {
  # datetime labels are locale dependent
  withr::local_locale(c(LC_TIME = "C"))

  df <- data_frame(
    dx = seq(as.POSIXct("2012-02-29 12:00:00",
      tz = "UTC",
      format = "%Y-%m-%d %H:%M:%S"
    ),
    length.out = 10, by = "4 hour"
    ),
    price = seq(20, 200000, length.out = 10)
  )
  df$date <- as.Date(df$dx)

  # date scale, dup_axis
  dt <- ggplot(df, aes(dx, price)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis())
  scale <- layer_scales(dt)$x
  breaks <- scale$break_info()
  expect_equal(breaks$major_source, breaks$sec.major_source_user)

  # datetime scale
  dt <- ggplot(df, aes(date, price)) +
    geom_line() +
    scale_x_date(sec.axis = dup_axis())
  scale <- layer_scales(dt)$x
  breaks <- scale$break_info()
  expect_equal(breaks$major_source, breaks$sec.major_source_user)

  # sec_axis
  dt <- ggplot(df, aes(dx, price)) +
    geom_line() +
    scale_x_datetime(
      name = "UTC",
      sec.axis = sec_axis(~ . + 12 * 60 * 60,
        name = "UTC+12"
      )
    )
  scale <- layer_scales(dt)$x
  breaks <- scale$break_info()

  expect_equal(
    as.numeric(breaks$major_source) + 12 * 60 * 60,
    as.numeric(breaks$sec.major_source_user)
  )

  # visual test, datetime scales, reprex #1936
  df <- data_frame(
    x = as.POSIXct(c(
      "2016-11-30 00:00:00",
      "2016-11-30 06:00:00",
      "2016-11-30 12:00:00",
      "2016-11-30 18:00:00",
      "2016-12-01 00:00:00"
    ), tz = "UTC"),
    y = c(0, -1, 0, 1, 0)
  )

  expect_doppelganger(
    "sec_axis, datetime scale",
    ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      scale_x_datetime("UTC",
        date_breaks = "2 hours", date_labels = "%I%p",
        sec.axis = dup_axis(~ . - 8 * 60 * 60, name = "PST")
      ) + theme_linedraw()
  )
})

test_that("sec.axis allows independent trans btwn primary and secondary axes", {
  data <- data_frame(
    Value = c(0.18, 0.29, 0.35, 0.46, 0.50, 0.50, 0.51),
    Probability = c(0.045, 0.090, 0.136, 0.181, 0.227, 0.272, 0.318)
  )
  expect_doppelganger(
    "sec_axis, independent transformations",
    ggplot(data = data, aes(Probability, Value)) + geom_point() +
      scale_x_continuous(
        trans = scales::probability_trans(distribution = "norm", lower.tail = FALSE),
        sec.axis = sec_axis(trans = ~ 1 / ., name = "Return Period")
      ) + theme_linedraw()
  )
})

# Currently fails do to necessary reversion of #2805
test_that("sec_axis() works for power transformations (monotonicity test doesn't fail)", {
  data <- data_frame(
    x = seq(0, 1, length.out = 100),
    y = seq(0, 4, length.out = 100)
  )
  expect_doppelganger(
    "sec axis monotonicity test",
    ggplot(data, aes(x, y)) +
      geom_line() +
      scale_y_continuous(trans = "sqrt", sec.axis = dup_axis()) + theme_linedraw()
  )

  testdat <- data_frame(
    x = runif(11),
    y = seq(0, 1, 0.1)
  )
  p <- ggplot(data = testdat, aes(x = x, y = y)) +
    geom_point() +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ .^0.5))
  scale <- layer_scales(p)$y
  breaks <- scale$break_info()
  expect_equal(breaks$major, sqrt(breaks$sec.major), tolerance = .005)

  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_x_sqrt(sec.axis = dup_axis())
  scale <- layer_scales(p)$x
  breaks <- scale$break_info()
  expect_equal(breaks$major, breaks$sec.major, tolerance = .001)

  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_x_sqrt(sec.axis = sec_axis(~ . * 100))
  scale <- layer_scales(p)$x
  breaks <- scale$break_info()
  expect_equal(breaks$major, breaks$sec.major, tolerance = .001)
})

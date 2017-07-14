context("scale_date")

base_time <- function(tz = "") {
  as.POSIXct(strptime("2015-06-01", "%Y-%m-%d", tz = tz))
}

df <- data.frame(
  time1 = base_time("") + 0:6 * 3600,
  time2 = base_time("UTC") + 0:6 * 3600,
  time3 = base_time("Australia/Lord_Howe") + (0:6 + 13) * 3600, # has half hour offset
  y = seq_along(base_time)
)

test_that("inherits timezone from data", {
  if (!is.null(attr(df$time1, "tzone")))
     skip("Local time zone not available")

  # Local time
  p <- ggplot(df, aes(y = y)) + geom_point(aes(time1))
  sc <- layer_scales(p)$x

  expect_true(identical(sc$timezone, NULL))
  expect_equal(sc$get_labels()[1], "00:00")

  # UTC
  p <- ggplot(df, aes(y = y)) + geom_point(aes(time2))
  sc <- layer_scales(p)$x
  expect_equal(sc$timezone, "UTC")
  expect_equal(sc$get_labels()[1], "00:00")
})


test_that("first timezone wins", {
  p <- ggplot(df, aes(y = y)) +
    geom_point(aes(time2)) +
    geom_point(aes(time3), colour = "red") +
    scale_x_datetime(date_breaks = "hour", date_labels = "%H:%M")
  sc <- layer_scales(p)$x
  expect_equal(sc$timezone, "UTC")
})

test_that("not cached across calls", {
  scale_x <- scale_x_datetime(date_breaks = "hour", date_labels = "%H:%M")

  p1 <- ggplot(df, aes(y = y)) + geom_point(aes(time2)) + scale_x
  p2 <- ggplot(df, aes(y = y)) + geom_point(aes(time3)) + scale_x

  expect_equal(layer_scales(p1)$x$timezone, "UTC")
  expect_equal(layer_scales(p2)$x$timezone, "Australia/Lord_Howe")
})

test_that("datetime size scales work", {
  p <- ggplot(df, aes(y = y)) + geom_point(aes(time1, size = time1))

  # Default size range is c(1, 6)
  expect_equal(range(layer_data(p)$size), c(1, 6))
})

test_that("datetime alpha scales work", {
  p <- ggplot(df, aes(y = y)) + geom_point(aes(time1, alpha = time1))

  # Default alpha range is c(0.1, 1.0)
  expect_equal(range(layer_data(p)$alpha), c(0.1, 1.0))
})

test_that("datetime colour scales work", {
  p <- ggplot(df, aes(y = y)) +
    geom_point(aes(time1, colour = time1)) +
    scale_colour_datetime()

  expect_equal(range(layer_data(p)$colour), c("#132B43", "#56B1F7"))
})

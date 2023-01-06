test_that("binned scales only support continuous data", {
  p <- ggplot(mtcars) + geom_bar(aes(as.character(gear))) + scale_x_binned()
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, colour = as.character(gear))) + scale_color_binned()
  expect_snapshot_error(ggplot_build(p))
})

test_that('binned scales can calculate breaks on dates', {

  data <- seq(as.Date("2000-01-01"), as.Date("2020-01-01"), length.out = 100)

  scale <- scale_x_binned(trans = "date")
  scale$train(scale$transform(data))
  breaks <- scale$trans$inverse(scale$get_breaks())

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

  scale <- scale_x_binned(trans = "time")
  scale$train(scale$transform(data))
  breaks <- scale$trans$inverse(scale$get_breaks())

  expect_s3_class(breaks, "POSIXct")
  expect_equal(
    unname(unclass(breaks)),
    unclass(as.POSIXct(strptime(
      paste0(seq(2002, 2018, by = 2), "-01-01"),
      "%Y-%m-%d"
    )))
  )
})

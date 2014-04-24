context("annotate")

test_that("dates in segment annotation work", {
  dt <-
    structure(list(month = structure(c(1356998400, 1359676800, 1362096000,
      1364774400, 1367366400, 1370044800, 1372636800, 1375315200, 1377993600,
      1380585600, 1383264000, 1385856000), tzone = "UTC", class = c("POSIXct",
      "POSIXt")), total = c(-5.4586134857604, -13.7043187714622, 3.25254944094792,
      -10.3260511491301, 6.44178978776937, 2.46127352381964, -1.98673390397442,
      -16.3227453308624, 11.7148615457185, -2.23934628094151, 5.5084335509504,
      -10.7700077344507)), row.names = c(NA, -12L), class = "data.frame",
      .Names = c("month", "total"))

  p <- qplot(month, total, data = dt)

  ggplot_gtable(ggplot_build(p + annotate("segment",
               x = as.POSIXct("2013-04-01"),
               xend = as.POSIXct("2013-07-01"),
               y = -10,
               yend = 10)))

})

test_that("segment annotations transform with scales", {
  # This should be a visual test, but contriubtion documentation does not
  # explain how to make one
  ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    annotate("segment", x=2, y=10, xend=5, yend=30, colour="red") +
    scale_y_reverse()
})

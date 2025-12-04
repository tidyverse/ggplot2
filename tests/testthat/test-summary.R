test_that("summary method gives a nice summary", {
  # This test isn't important enough to break anything on CRAN
  skip_on_cran()

  p <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
    geom_point() +
    scale_x_continuous() +
    scale_colour_brewer() +
    facet_grid(year ~ cyl)

  expect_snapshot(summary(p))
})

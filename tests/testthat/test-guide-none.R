skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("guide_none() can be used in non-position scales", {
  p <- ggplot(mpg, aes(cty, hwy, colour = class)) +
    geom_point() +
    scale_color_discrete(guide = guide_none())

  built <- ggplot_build(p)
  plot <- built@plot
  guides <- guides_list(plot@guides)
  guides <- guides$build(
    plot@scales,
    plot@layers,
    plot@labels
  )

  expect_length(guides$guides, 0)
})

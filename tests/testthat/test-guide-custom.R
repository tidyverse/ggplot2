skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("guide_custom can be drawn and styled", {
  p <- ggplot() +
    guides(
      custom = guide_custom(
        circleGrob(r = unit(1, "cm")),
        title = "custom guide"
      )
    )

  expect_doppelganger(
    "stylised guide_custom",
    p +
      theme(
        legend.background = element_rect(fill = "grey50"),
        legend.title.position = "left",
        legend.title = element_text(angle = 90, hjust = 0.5)
      )
  )

  expect_doppelganger(
    "guide_custom with void theme",
    p + theme_void()
  )
})

test_that("aesthetic checking in geom throws correct errors", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, colour = after_scale(data)))
  expect_snapshot_error(ggplotGrob(p))

  aes <- list(a = 1:4, b = letters[1:4], c = TRUE, d = 1:2, e = 1:5)
  expect_snapshot_error(check_aesthetics(aes, 4))
})

test_that("layer clipping is applied correctly", {
  base <- ggplot() +
    geom_segment(
      aes(
        x = -1,
        xend = 2,
        y = 0,
        yend = 0,
        colour = "on"
      ),
      clip = "on"
    ) +
    geom_segment(
      aes(
        x = -1,
        xend = 2,
        y = 1,
        yend = 1,
        colour = "off"
      ),
      clip = "off"
    ) +
    geom_segment(
      aes(
        x = -1,
        xend = 2,
        y = 2,
        yend = 2,
        colour = "inherit"
      ),
      clip = "inherit"
    ) +
    theme_test() +
    theme(legend.position = "bottom")

  expect_doppelganger(
    "coord_cartesian(clip = 'on')",
    base + coord_cartesian(clip = 'on')
  )

  expect_doppelganger(
    "coord_cartesian(clip = 'off')",
    base + coord_cartesian(clip = 'off')
  )
})

test_that("secondary labels are correctly turned off", {
  # Using a visual test because the labels are only generated during rendering
  expect_doppelganger("turning off secondary title with coord_flip",
    ggplot(mtcars, aes(x = mpg, y = cyl)) +
      geom_point() +
      scale_x_continuous(sec.axis = dup_axis(guide = guide_axis(title = NULL))) +
      coord_flip() +
      theme_test() +
      theme(
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.major.x = element_line(colour = "grey90")
      )
  )
})

test_that("flip coords throws error when limits are badly specified", {
  # throws error when limit is a Scale object instead of vector
  expect_snapshot_error(ggplot() + coord_flip(xlim(1,1)))

  # throws error when limit's length is different than two
  expect_snapshot_error(ggplot() + coord_flip(ylim=1:3))
})

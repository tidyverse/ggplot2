test_that("secondary labels are correctly turned off", {
  # Using a visual test because the labels are only generated during rendering
  expect_doppelganger("turning off secondary title with coord_flip",
    ggplot(mtcars, aes(x = mpg, y = cyl)) +
      geom_point() +
      scale_x_continuous(sec.axis = dup_axis(guide = guide_axis(title = NULL))) +
      coord_flip()
  )
})

test_that("flip coords throws error when limits are badly specified", {
  # throws error when xlim is environment
  expect_snapshot_error(ggplot() + coord_flip(xlim(1,1)))

  # throws error when ylim is environment
  expect_snapshot_error(ggplot() + coord_flip(ylim=1:3))
})

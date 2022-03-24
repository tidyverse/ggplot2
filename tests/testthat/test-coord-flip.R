test_that("secondary labels are correctly turned off", {
  # Using a visual test because the labels are only generated during rendering
  expect_doppelganger("turning off secondary title with coord_flip",
    ggplot(mtcars, aes(x = mpg, y = cyl)) +
      geom_point() +
      scale_x_continuous(sec.axis = dup_axis(guide = guide_axis(title = NULL))) +
      coord_flip()
  )
})

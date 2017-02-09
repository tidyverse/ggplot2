context("coord_cartesian")

# Visual tests ------------------------------------------------------------

test_that("cartesian coords draws correctly with limits", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

  vdiffr::expect_doppelganger("expand range",
    p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 50))
  )
  vdiffr::expect_doppelganger("contract range",
    p + coord_cartesian(xlim = c(2, 4), ylim = c(20, 40))
  )
})

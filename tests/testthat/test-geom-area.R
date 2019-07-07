context("geom_area")

test_that("geom_area can cross 0", {
  d <- data_frame(
    x = 1:6,
    y = c(1, 2, 2, 1, 0, -1)
  )

  expect_doppelganger("negative geom area", ggplot(d, aes(x, y)) + geom_area())
})

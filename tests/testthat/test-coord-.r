test_that("clipping is on by default", {
  p <- ggplot()
  coord <- ggplot_build(p)$layout$coord
  expect_equal(coord$clip, "on")
})

test_that("message when replacing non-default coordinate system", {

  df <- data_frame(x = 1, y = 2)
  gg <- ggplot(df, aes(x, y))

  expect_message(gg + coord_cartesian(), NA)
  expect_message(
    gg + coord_cartesian() + coord_cartesian(),
    "Adding new coordinate system"
  )
  
})

context("test-coord-.r")

test_that("message when replacing non-default coordinate system", {

  df <- data.frame(x = 1, y = 2)
  gg <- ggplot(df, aes(x, y))

  expect_message(gg + coord_cartesian(), NA)
  expect_message(
    gg + coord_cartesian() + coord_cartesian(),
    "Adding new coordinate system"
  )

  expect_equal(2 * 2, 4)
})

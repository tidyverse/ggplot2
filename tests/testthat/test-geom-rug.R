context("geom_rug")

n = 10
df <- data.frame(x = 1:n, y = (1:n)^3)
p <- ggplot(df, aes(x, y)) + geom_point() + geom_rug(sides = 'l')

test_that("coord_flip flips the rugs", {
  a <- layer_grob(p, 2)
  b <- layer_grob(p + coord_flip(), 2)

  # Rugs along y-axis, all x coordinates are the same
  expect_equal(length(a[[1]]$children[[1]]$x0), 1)
  expect_equal(length(a[[1]]$children[[1]]$x1), 1)
  expect_equal(length(a[[1]]$children[[1]]$y0), n)
  expect_equal(length(a[[1]]$children[[1]]$y1), n)

  # Rugs along x-axis, all y coordinates are the same
  expect_equal(length(b[[1]]$children[[1]]$x0), n)
  expect_equal(length(b[[1]]$children[[1]]$x1), n)
  expect_equal(length(b[[1]]$children[[1]]$y0), 1)
  expect_equal(length(b[[1]]$children[[1]]$y1), 1)
})

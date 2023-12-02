n = 10
df <- data_frame(x = 1:n, y = (1:n)^3)
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

test_that("Rug length needs unit object", {
  p <- ggplot(df, aes(x,y))
  expect_snapshot_error(print(p + geom_rug(length = 0.01)))
})

test_that("Rug lengths are correct", {
  a <- layer_grob(p, 2)

  # Check default lengths
  expect_equal(a[[1]]$children[[1]]$x0, unit(0, "npc"))
  expect_equal(a[[1]]$children[[1]]$x1, unit(0.03, "npc"))

  p <- ggplot(df, aes(x, y)) + geom_point() + geom_rug(sides = 'l', length = unit(12, "pt"))
  b <- layer_grob(p, 2)

  # Check default length is changed
  expect_equal(a[[1]]$children[[1]]$x0, unit(0, "npc"))
  expect_equal(b[[1]]$children[[1]]$x1, unit(12, "pt"))

})


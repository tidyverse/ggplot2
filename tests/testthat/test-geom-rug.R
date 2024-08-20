n = 10
df <- data_frame(x = 1:n, y = (1:n)^3)
p <- ggplot(df, aes(x, y)) + geom_point() + geom_rug(sides = 'l')

test_that("coord_flip flips the rugs", {
  a <- get_layer_grob(p, 2)
  b <- get_layer_grob(p + coord_flip(), 2)

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
  a <- get_layer_grob(p, 2)

  # Check default lengths
  expect_equal(a[[1]]$children[[1]]$x0, unit(0, "npc"))
  expect_equal(a[[1]]$children[[1]]$x1, unit(0.03, "npc"))

  p <- ggplot(df, aes(x, y)) + geom_point() + geom_rug(sides = 'l', length = unit(12, "pt"))
  b <- get_layer_grob(p, 2)

  # Check default length is changed
  expect_equal(a[[1]]$children[[1]]$x0, unit(0, "npc"))
  expect_equal(b[[1]]$children[[1]]$x1, unit(12, "pt"))

})

test_that(
  "geom_rug() warns about missing values when na.rm = FALSE",
  {
    df2 <- df
    n_missing <- 2
    df2$x[sample(nrow(df2), size = n_missing)] <- NA

    p1 <- ggplot(df2, aes(x = x)) + geom_rug()
    p2 <- ggplot(df2, aes(x = x)) + geom_rug(na.rm = TRUE)

    expect_warning(
      ggplotGrob(p1),
      paste0("Removed ", n_missing, " rows containing missing values or values outside the scale range")
    )

    expect_no_warning(ggplotGrob(p2))
  }
)

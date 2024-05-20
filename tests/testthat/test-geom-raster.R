test_that("geom_raster() checks input and coordinate system", {
  expect_snapshot_error(geom_raster(hjust = c(2.5, 1.3)))
  expect_snapshot_error(geom_raster(hjust = "a"))
  expect_snapshot_error(geom_raster(vjust = c(2.5, 1.3)))
  expect_snapshot_error(geom_raster(vjust = "a"))

  df <- data_frame(x = rep(c(-1, 1), each = 3), y = rep(-1:1, 2), z = 1:6)
  p <- ggplot(df, aes(x, y, fill = z)) + geom_raster() + coord_polar()
  expect_snapshot_error(ggplotGrob(p))
})

test_that("geom_raster() fails with pattern fills", {
  skip_if_not(getRversion() > "4.2", message = "pattern fills are unavailalbe")
  df <- data.frame(x = 1)
  p <- ggplot(df, aes(x, x)) + geom_raster(fill = linearGradient())
  expect_snapshot_error(ggplotGrob(p))
})

# Visual tests ------------------------------------------------------------

test_that("geom_raster draws correctly", {
  set.seed(1)

  # 3 x 2 ----------------------------------------------------------------------
  df <- data_frame(x = rep(c(-1, 1), each = 3), y = rep(-1:1, 2), z = 1:6)

  expect_doppelganger("3 x 2",
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red")
  )
  expect_doppelganger("3 x 2, set limits",
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") +
      xlim(-2, 2) + ylim(-2, 2)
  )
  expect_doppelganger("3 x 2, just = (0, 0)",
    ggplot(df, aes(x, y, fill = z)) + geom_raster(hjust = 0, vjust = 0) +
      geom_point(colour = "red")
  )

  # 1 x 3 ----------------------------------------------------------------------
  df <- data_frame(x = -1:1, y = 0, z = 1:3)

  expect_doppelganger("1 x 3",
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red")
  )
  expect_doppelganger("1 x 3, set limits",
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") +
      xlim(-2, 2) + ylim(-2, 2)
  )
  expect_doppelganger("1 x 3, just = (0, 0)",
    ggplot(df, aes(x, y, fill = z)) + geom_raster(hjust = 0, vjust = 0) +
      geom_point(colour = "red")
  )

  # 3 x 1 ----------------------------------------------------------------------

  df <- data_frame(x = 0, y = -1:1, z = 1:3)
  expect_doppelganger("3 x 1",
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red")
  )
  expect_doppelganger("3 x 1, set limits",
    ggplot(df, aes(x, y, fill = z)) + geom_raster() + geom_point(colour = "red") +
      xlim(-2, 2) + ylim(-2, 2)
  )
  expect_doppelganger("3 x 1, just = (0, 0)",
    ggplot(df, aes(x, y, fill = z)) + geom_raster(hjust = 0, vjust = 0) +
      geom_point(colour = "red")
  )

  # Categorical fill, irregular swatches ---------------------------------------

  df <- expand.grid(x = 1:10, y = 1:10)
  df$col <- (df$x + df$y) %% 2
  df$col[df$x == 5 & df$col == 1] <- NA
  df$col[df$y == 5 & df$col == 0] <- NA
  expect_doppelganger("irregular categorical",
    ggplot(df, aes(x, y, fill = factor(col))) + geom_raster()
  )

  # Categorical axes -----------------------------------------------------------

  df <- expand.grid(x = c("A", "B"), y = c("C", "D"))
  expect_doppelganger("discrete positions",
    ggplot(df, aes(x, y, fill = interaction(x, y))) + geom_raster()
  )
})

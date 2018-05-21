context("geom-raster")


# Visual tests ------------------------------------------------------------

test_that("geom_raster draws correctly", {
  set.seed(1)

  # 3 x 2 ----------------------------------------------------------------------
  df <- data.frame(x = rep(c(-1, 1), each = 3), y = rep(-1:1, 2), z = 1:6)

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
  df <- data.frame(x = -1:1, y = 0, z = 1:3)

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

  df <- data.frame(x = 0, y = -1:1, z = 1:3)
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
    qplot(x, y, data = df, fill = factor(col), geom = "raster")
  )
})

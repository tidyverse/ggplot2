skip_if_not_installed("hexbin")

test_that("density and value summaries are available", {
  df <- data_frame(x = c(1, 1, 1, 2), y = c(1, 1, 1, 2))
  base <- ggplot(df, aes(x, y)) +
    geom_hex()

  out <- get_layer_data(base)
  expect_equal(nrow(out), 2)
  expect_equal(out$density, c(0.75, 0.25), tolerance = 1e-7)
  expect_equal(out$count, c(3, 1), tolerance = 1e-7)
})

test_that("size and linetype are applied", {
  df <- data_frame(x = c(1, 1, 1, 2), y = c(1, 1, 1, 2))
  plot <- ggplot(df, aes(x, y)) +
    geom_hex(color = "red", linewidth = 4, linetype = 2)

  gpar <- get_layer_grob(plot)[[1]]$children[[1]]$gp
  expect_equal(gpar$lwd, rep(4, 2) * .pt, tolerance = 1e-7)
  expect_equal(gpar$lty, rep(2, 2), tolerance = 1e-7)
})

test_that("bin size are picked up from stat", {
  expect_doppelganger("single hex bin with width and height of 0.1",
    ggplot(data.frame(x = 0, y = 0)) +
      geom_hex(aes(x = x, y = y), binwidth = c(0.1, 0.1)) +
      coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))
  )
})

test_that("geom_hex works in non-linear coordinate systems", {
  p <- ggplot(mpg, aes(displ, hwy)) + geom_hex()

  expect_doppelganger("hex bin plot with sqrt-transformed y",
    p + coord_trans(y = "sqrt")
  )
  expect_doppelganger("hex bin plot in polar coordinates",
                      p + coord_polar()
  )
})

context("geom_hex")

test_that("density and value summaries are available", {
  df <- data.frame(x = c(1, 1, 1, 2), y = c(1, 1, 1, 2))
  base <- ggplot(df, aes(x, y)) +
    geom_hex()

  out <- layer_data(base)
  expect_equal(nrow(out), 2)
  expect_equal(out$density, c(0.75, 0.25), tolerance = 1e-7)
  expect_equal(out$count, c(3, 1), tolerance = 1e-7)
})

test_that("size and linetype are applied", {
  df <- data.frame(x = c(1, 1, 1, 2), y = c(1, 1, 1, 2))
  plot <- ggplot(df, aes(x, y)) +
    geom_hex(color = "red", size = 4, linetype = 2)

  gpar <- layer_grob(plot)[[1]]$children[[1]]$gp
  expect_equal(gpar$lwd, c(4, 4) * .pt, tolerance = 1e-7)
  expect_equal(gpar$lty, c(2, 2), tolerance = 1e-7)
})

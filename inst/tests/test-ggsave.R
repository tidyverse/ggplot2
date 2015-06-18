context("ggsave")


# plot_dim ---------------------------------------------------------------

test_that("guesses and informs if dim not specified", {
  png(width = 10, height = 10, units = "in", res = 300)
  on.exit(capture.output(dev.off()))

  expect_message(out <- plot_dim(), "10 x 10")
  expect_equal(out, c(10, 10))
})

test_that("warned about large plot unless limitsize = FALSE", {
  expect_error(plot_dim(c(50, 50)), "exceed 50 inches")
  expect_equal(plot_dim(c(50, 50), limitsize = FALSE), c(50, 50))
})

test_that("scale multiplies height & width", {
  expect_equal(plot_dim(c(10, 10), scale = 1), c(10, 10))
  expect_equal(plot_dim(c(5, 5), scale = 2), c(10, 10))
})

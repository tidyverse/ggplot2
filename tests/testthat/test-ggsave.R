context("ggsave")

test_that("ggsave creates file", {
  path <- tempfile()
  on.exit(unlink(path))

  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

  expect_false(file.exists(path))
  ggsave(path, p, device = "pdf", width = 5, height = 5)
  expect_true(file.exists(path))
})


# plot_dim ---------------------------------------------------------------

test_that("guesses and informs if dim not specified", {
  png(width = 10, height = 10, units = "in", res = 300)
  on.exit(capture.output(dev.off()))

  expect_message(out <- plot_dim(), "10 x 10")
  expect_equal(out, c(10, 10))
})

test_that("uses 7x7 if no graphics device open", {
  expect_equal(plot_dim(), c(7, 7))
})

test_that("warned about large plot unless limitsize = FALSE", {
  expect_error(plot_dim(c(50, 50)), "exceed 50 inches")
  expect_equal(plot_dim(c(50, 50), limitsize = FALSE), c(50, 50))
})

test_that("scale multiplies height & width", {
  expect_equal(plot_dim(c(10, 10), scale = 1), c(10, 10))
  expect_equal(plot_dim(c(5, 5), scale = 2), c(10, 10))
})


# plot_dev ---------------------------------------------------------------------

test_that("function passed back unchanged", {
  expect_equal(plot_dev(png), png)
})

test_that("unknown device triggers error", {
  expect_error(plot_dev("xyz"), "Unknown graphics device")
  expect_error(plot_dev(NULL, "test.xyz"), "Unknown graphics device")
})


test_that("text converted to function", {
  expect_identical(body(plot_dev("png"))[[1]], quote(grDevices::png))
  expect_identical(body(plot_dev("pdf"))[[1]], quote(grDevices::pdf))
})

test_that("if device is NULL, guess from extension", {
  expect_identical(body(plot_dev(NULL, "test.png"))[[1]], quote(grDevices::png))
})

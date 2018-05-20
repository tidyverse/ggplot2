context("ggsave")

test_that("ggsave creates file", {
  path <- tempfile()
  on.exit(unlink(path))

  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

  expect_false(file.exists(path))
  ggsave(path, p, device = "pdf", width = 5, height = 5)
  expect_true(file.exists(path))
})

test_that("ggsave restores previous graphics device", {
  # When multiple devices are open, dev.off() restores the next one in the list,
  # not the previously-active one. (#2363)
  path <- tempfile()
  on.exit(unlink(path))

  png(tempfile())
  png(tempfile())
  on.exit(graphics.off(), add = TRUE)

  old_dev <- dev.cur()
  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  ggsave(path, p, device = "png", width = 5, height = 5)

  expect_identical(old_dev, dev.cur())
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

test_that("function is passed back unchanged", {
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

# parse_dpi ---------------------------------------------------------------

test_that("DPI string values are parsed correctly", {
  expect_type(parse_dpi("print"), "double")
  expect_type(parse_dpi("screen"), "double")
  expect_type(parse_dpi("retina"), "double")
  expect_type(parse_dpi(100), "double")
  expect_type(parse_dpi(300L), "integer")
})

test_that("invalid single-string DPI values throw an error", {
  expect_error(parse_dpi("abc"), "Unknown DPI string")
})

test_that("invalid non-single-string DPI values throw an error", {
  expect_error(parse_dpi(factor(100)), "DPI must be a single number or string")
  expect_error(parse_dpi(c("print", "screen")), "DPI must be a single number or string")
  expect_error(parse_dpi(c(150, 300)), "DPI must be a single number or string")
  expect_error(parse_dpi(list(150)), "DPI must be a single number or string")
})

test_that("ggsave creates file", {
  path <- tempfile()
  on.exit(unlink(path))

  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

  expect_false(file.exists(path))
  ggsave(path, p, device = "pdf", width = 5, height = 5)
  expect_true(file.exists(path))
})

test_that("ggsave can create directories", {
  dir <- tempdir()
  path <- file.path(dir, "foobar", "tmp.pdf")
  on.exit(unlink(path))

  p <- ggplot(mpg, aes(displ, hwy)) + geom_point()

  expect_error(ggsave(path, p))
  expect_false(dir.exists(dirname(path)))

  # 2 messages: 1 for saving and 1 informing about directory creation
  expect_message(expect_message(ggsave(path, p, create.dir = TRUE)))
  expect_true(dir.exists(dirname(path)))
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

test_that("ggsave uses theme background as image background", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("xml2")

  path <- tempfile()
  on.exit(unlink(path))
  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    coord_fixed() +
    theme(plot.background = element_rect(fill = "#00CCCC"))
  ggsave(path, p, device = "svg", width = 5, height = 5)
  img <- xml2::read_xml(path)
  # Find background rect in svg
  bg <- as.character(xml2::xml_find_first(img, xpath = "d1:rect/@style"))
  expect_true(grepl("fill: #00CCCC", bg))
})

test_that("ggsave can handle blank background", {
  skip_if_not_installed("svglite")
  skip_if_not_installed("xml2")

  path <- tempfile()
  on.exit(unlink(path))
  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    theme(plot.background = element_blank())
  ggsave(path, p, device = "svg", width = 5, height = 5)
  img <- xml2::read_xml(path)
  bg <- as.character(xml2::xml_find_first(img, xpath = "d1:rect/@style"))
  expect_true(grepl("fill: none", bg))
})

test_that("ggsave warns about empty or multiple filenames", {
  plot <- ggplot(mtcars, aes(disp, mpg)) + geom_point()

  withr::with_tempfile(c("file1", "file2"), fileext = ".png", {
    expect_warning(
      suppressMessages(ggsave(c(file1, file2), plot)),
      "`filename` must have length 1"
    )
  })

  expect_error(
    ggsave(character(), plot),
    "`filename` must be a single string"
  )
})

test_that("ggsave fails informatively for no-extension filenames", {
  plot <- ggplot(mtcars, aes(disp, mpg)) + geom_point()
  expect_error(
    ggsave(tempfile(), plot),
    '`filename` has no file extension and `device` is "NULL"'
  )
})

# plot_dim ---------------------------------------------------------------

test_that("guesses and informs if dim not specified", {
  png(width = 10, height = 10, units = "in", res = 300)
  on.exit(capture.output(dev.off()))

  expect_message(out <- plot_dim(), "10 x 10")
  expect_equal(out, c(10, 10))
})

test_that("uses 7x7 if no graphics device open", {
  suppressMessages(expect_equal(plot_dim(), c(7, 7)))
})

test_that("warned about large plot unless limitsize = FALSE", {
  expect_error(plot_dim(c(50, 50)), "exceed 50 inches")
  expect_equal(plot_dim(c(50, 50), limitsize = FALSE), c(50, 50))
  expect_error(plot_dim(c(15000, 15000), units = "px"), "in pixels).")
})

test_that("scale multiplies height & width", {
  expect_equal(plot_dim(c(10, 10), scale = 1), c(10, 10))
  expect_equal(plot_dim(c(5, 5), scale = 2), c(10, 10))
})

# plot_dev ---------------------------------------------------------------------

test_that("unknown device triggers error", {
  expect_snapshot_error(plot_dev(1))
  expect_error(plot_dev("xyz"), "Unknown graphics device")
  expect_error(plot_dev(NULL, "test.xyz"), "Unknown graphics device")
})


test_that("text converted to function", {
  expect_identical(body(plot_dev("png"))[[1]], quote(png_dev))
  expect_identical(body(plot_dev("pdf"))[[1]], quote(grDevices::pdf))
})

test_that("if device is NULL, guess from extension", {
  expect_identical(body(plot_dev(NULL, "test.png"))[[1]], quote(png_dev))
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
  expect_snapshot_error(parse_dpi("abc"))
})

test_that("invalid non-single-string DPI values throw an error", {
  expect_snapshot_error(parse_dpi(factor(100)))
  expect_snapshot_error(parse_dpi(c("print", "screen")))
  expect_snapshot_error(parse_dpi(c(150, 300)))
  expect_snapshot_error(parse_dpi(list(150)))
})

context("stat_bar/stat_bin")

test_that("stat_bin throws error when y aesthetic present", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_error(ggplot_build(ggplot(dat, aes(x, y)) + stat_bin()),
    "must not be used with a y aesthetic.")

  expect_error(p <- ggplot_build(ggplot(dat, aes(x)) + stat_bin(y = 5)),
    "Unknown parameters: y")
})

test_that("stat_bar throws error when y aesthetic present", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_error(ggplot_build(ggplot(dat, aes(x, y)) + stat_bar()),
    "must not be used with a y aesthetic.")

  expect_error(p <- ggplot_build(ggplot(dat, aes(x)) + stat_bar(y = 5)),
    "Unknown parameters: y")
})

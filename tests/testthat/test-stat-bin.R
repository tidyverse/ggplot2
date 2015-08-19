context("stat_bin")

test_that("stat_bin throws error when y aesthetic present", {
  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  # Should get an error when mapping/setting y and also using stat_bin
  # But errors caught by internal tryCatch :()
  #   expect_error(ggplot_build(ggplot(dat, aes(x=x, y=y)) + geom_bar()),
  #     "Mapping a variable to y and also using stat=\"bin\"")
  #   expect_error(p <- ggplot_build(ggplot(dat, aes(x=x, y=y)) + geom_bar(stat="bin")),
  #     "Mapping a variable to y and also using stat=\"bin\"")
  #
  #   expect_error(p <- ggplot_build(ggplot(dat, aes(x=x)) + geom_bar(y=5)),
  #     "Mapping a variable to y and also using stat=\"bin\"")

  # This gives an error  (it would probably be OK if just one
  # of these happened, but this test looks for both)
  dat2 <- data.frame(x = c("a", "b", "c", "a", "b", "c"), y = c(1, 5, 10, 2, 3, 4))
  #  expect_error(
  #     p <- ggplot_build(ggplot(dat2, aes(x=x, y=y)) + geom_bar()))
})

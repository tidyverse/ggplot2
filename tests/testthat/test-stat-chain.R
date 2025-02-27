test_that("stat_chain can chain multiple stats", {

  df <- data.frame(x = c(1, 1.9, 2.1, 3, 3, 3))

  p <- ggplot(df, aes(x)) +
    stat_chain(
      stats = list(link_stat("bin", breaks = 0.5:3.5))
    ) +
    stat_chain(
      stats = list("unique", link_stat("bin", breaks = 0.5:3.5)),
    ) +
    stat_chain(
      stats = list("unique", link_stat("bin", breaks = 0.5:3.5, after.stat = aes(y = -count)))
    )
  p <- ggplot_build(p)

  ld <- get_layer_data(p, 1L)
  expect_equal(ld$x, 1:3)
  expect_equal(ld$y, 1:3)

  ld <- get_layer_data(p, 2L)
  expect_equal(ld$x, 1:3)
  expect_equal(ld$y, c(1, 2, 1))

  ld <- get_layer_data(p, 3L)
  expect_equal(ld$x, 1:3)
  expect_equal(ld$y, c(-1, -2, -1))
})

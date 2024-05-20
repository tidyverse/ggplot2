test_that("stat_ellipsis returns correct data format", {
  n_seg <- 40
  d <- data_frame(x = c(1, 1, 4, 4, 4, 3, 3, 1), y = c(1:4, 1:4), id = rep(1:2, each = 4))
  p <- ggplot(d, aes(x = x, y = y, group = id)) +
    geom_point() +
    stat_ellipse(segments = n_seg)
  out <- get_layer_data(p, 2)
  expect_equal(nrow(out), (n_seg + 1) * 2)
  expect_equal(unique(out$group), c(1, 2))
})

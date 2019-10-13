context("geom_ribbon")

test_that("NAs are not dropped from the data", {
  df <- data_frame(x = 1:5, y = c(1, 1, NA, 1, 1))

  p <- ggplot(df, aes(x))+
    geom_ribbon(aes(ymin = y - 1, ymax = y + 1))

  expect_equal(layer_data(p)$ymin, c(0, 0, NA, 0, 0))
})

test_that("geom_ribbon works in both directions", {
  dat <- data_frame(x = seq_len(5),
                    ymin = c(1, 2, 1.5, 1.8, 1),
                    ymax = c(4, 6, 5, 4.5, 5.2))

  p <- ggplot(dat, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon()
  x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(dat, aes(y = x, xmin = ymin, xmax = ymax)) + geom_ribbon()
  y <- layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])
})

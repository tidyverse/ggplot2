context("stat_hex")

test_that("can use length 1 binwidth", {
  df <- data.frame(x = c(1, 1, 2), y = c(1, 1, 2))
  p <- ggplot(df, aes(x, y)) + stat_binhex(binwidth = 1)

  expect_equal(nrow(layer_data(p)), 2)
})

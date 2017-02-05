context("position_dodge")

test_that("can control whether to preserve total or individual width", {
  df <- data.frame(x = c("a", "b", "b"), y = c("a", "a", "b"))

  p_total <- ggplot(df, aes(x, fill = y)) +
    geom_bar(position = position_dodge(preserve = "total"), width = 1)
  p_single <- ggplot(df, aes(x, fill = y)) +
    geom_bar(position = position_dodge(preserve = "single"), width = 1)

  expect_equal(layer_data(p_total)$x, c(1, 2.25, 1.75))
  expect_equal(layer_data(p_single)$x, c(0.75, 2.25, 1.75))
})

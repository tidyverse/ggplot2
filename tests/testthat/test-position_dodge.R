test_that("can control whether to preserve total or individual width", {
  df <- data_frame(x = c("a", "b", "b"), y = c("a", "a", "b"))

  p_total <- ggplot(df, aes(x, fill = y)) +
    geom_bar(position = position_dodge(preserve = "total"), width = 1)
  p_single <- ggplot(df, aes(x, fill = y)) +
    geom_bar(position = position_dodge(preserve = "single"), width = 1)

  expect_equal(layer_data(p_total)$x, new_mapped_discrete(c(1, 1.75, 2.25)))
  expect_equal(layer_data(p_single)$x, new_mapped_discrete(c(0.75, 1.75, 2.25)))
})

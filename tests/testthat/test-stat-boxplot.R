context("stat_boxplot")

test_that("stat_boxplot drops missing rows with a warning", {

  p1 <- ggplot(PlantGrowth, aes(x = group, y = weight)) +
    geom_boxplot(position = "dodge") +
    scale_x_discrete(limits = c("trt1", "ctrl"))

  p2 <- ggplot(PlantGrowth, aes(x = group, y = weight)) +
    geom_boxplot(position = "dodge2") +
    scale_x_discrete(limits = c("trt1", "ctrl"))

  expect_warning(
    ggplot_build(p1),
    "Removed 10 rows containing missing values \\(stat_boxplot\\)\\."
  )
  expect_warning(
    ggplot_build(p2),
    "Removed 10 rows containing missing values \\(stat_boxplot\\)\\."
  )
})

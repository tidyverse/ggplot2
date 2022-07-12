test_that("stat_boxplot drops missing rows with a warning", {

  p1 <- ggplot(PlantGrowth, aes(x = group, y = weight)) +
    geom_boxplot(position = "dodge") +
    scale_x_discrete(limits = c("trt1", "ctrl"))

  p2 <- ggplot(PlantGrowth, aes(x = group, y = weight)) +
    geom_boxplot(position = "dodge2") +
    scale_x_discrete(limits = c("trt1", "ctrl"))

  expect_snapshot_warning(ggplot_build(p1))
  expect_snapshot_warning(ggplot_build(p2))
})

test_that("stat_boxplot can suppress warning about missing rows", {
  p1 <- ggplot(PlantGrowth, aes(x = group, y = weight)) +
    geom_boxplot(position = "dodge", na.rm = TRUE) +
    scale_x_discrete(limits = c("trt1", "ctrl"))

  expect_silent(ggplot_build(p1))
})

test_that("stat_boxplot errors with missing x/y aesthetics", {
  p <- ggplot(PlantGrowth) +
    geom_boxplot()
  expect_snapshot_error(ggplot_build(p))
})

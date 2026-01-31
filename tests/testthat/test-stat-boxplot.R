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

test_that("stat_boxplot respects the `min.group.n` setting", {
  df <- data.frame(x = rep(c("A", "B"), c(3, 7)), y = c(1:10))
  ld <- layer_data(
    ggplot(df, aes(x, y)) + geom_boxplot(min.group.n = 5)
  )
  expect_equal(lengths(ld$outliers), c(3, 0))
  expect_equal(ld$middle, c(NA, 7))
})

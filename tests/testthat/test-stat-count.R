test_that("stat_count() checks the aesthetics", {
  p <- ggplot(mtcars) + stat_count()
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + stat_count(aes(factor(gear), mpg))
  expect_snapshot_error(ggplot_build(p))
})

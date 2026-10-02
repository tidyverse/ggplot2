test_that("geom_spoke warns about infinite radius (#6671)", {
  p <- ggplot() + geom_spoke(aes(0, 0, angle = pi / 4, radius = Inf))
  expect_snapshot_warning(ggplot_build(p))
})

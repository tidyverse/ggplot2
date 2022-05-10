test_that("ggplot() throws informative errors", {
  expect_snapshot_error(ggplot(mapping = letters))
  expect_snapshot_error(ggplot(data))
})

test_that("construction have user friendly errors", {
  expect_snapshot_error(+ geom_point())
  expect_snapshot_error(geom_point() + geom_bar())
  expect_snapshot_error(ggplot() + 1)
  expect_snapshot_error(ggplot() + geom_point)
})

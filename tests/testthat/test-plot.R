test_that("ggplot() throws informative errors", {
  expect_snapshot(error = TRUE, ggplot(mapping = letters))
  expect_snapshot(error = TRUE, ggplot(data))
})

test_that("construction have user friendly errors", {
  expect_snapshot(error = TRUE, + geom_point())
  expect_snapshot(error = TRUE, geom_point() + geom_bar())
  expect_snapshot(error = TRUE, ggplot() + 1)
  expect_snapshot(error = TRUE, ggplot() + geom_point)
})

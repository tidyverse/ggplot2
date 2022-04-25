test_that("ggplot() throws informative errors", {
  expect_snapshot_error(ggplot(mapping = letters))
  expect_snapshot_error(ggplot(data))
})

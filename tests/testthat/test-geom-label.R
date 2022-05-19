test_that("geom_label() throws meaningful errors", {
  expect_snapshot_error(geom_label(position = "jitter", nudge_x = 0.5))
  expect_snapshot_error(labelGrob(label = 1:3))
})

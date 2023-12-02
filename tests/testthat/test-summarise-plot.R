test_that("summarise_*() throws appropriate errors", {

  expect_snapshot_error(summarise_layout(10))
  expect_snapshot_error(summarise_coord("A"))
  expect_snapshot_error(summarise_layers(TRUE))

})

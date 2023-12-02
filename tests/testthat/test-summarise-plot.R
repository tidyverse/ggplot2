test_that("summarise_*() throws appropriate errors", {

  expect_snapshot(error = TRUE, summarise_layout(10))
  expect_snapshot(error = TRUE, summarise_coord("A"))
  expect_snapshot(error = TRUE, summarise_layers(TRUE))

})

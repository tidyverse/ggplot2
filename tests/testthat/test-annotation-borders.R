test_that("annotation_borders() can create a map", {
  skip_if_not_installed("maps")
  lifecycle::expect_deprecated(utah <- borders("state", "utah"))
  expect_doppelganger("annotation_borders utah", ggplot() + utah)
})

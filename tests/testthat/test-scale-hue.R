test_that("scale_hue() checks the type input", {
  expect_snapshot_error(pal_qualitative(type = 1:4))
  pal <- pal_qualitative(type = colors())
  expect_silent(pal(4))
  pal <- pal_qualitative(type = list(colors()[1:10], colors()[11:30]))
  expect_silent(pal(4))
})

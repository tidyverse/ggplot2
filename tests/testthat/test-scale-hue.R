test_that("scale_hue() checks the type input", {
  pal <- qualitative_pal(type = 1:4)
  expect_snapshot(error = TRUE, pal(4))
  pal <- qualitative_pal(type = colors())
  expect_silent(pal(4))
  pal <- qualitative_pal(type = list(colors()[1:10], colors()[11:30]))
  expect_silent(pal(4))
})

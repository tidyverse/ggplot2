test_that("basic edition requirement and deprecation works as intended", {

  local_edition(2025)
  expect_snapshot(edition_deprecate(2025, what = "foo()"), error = TRUE)
  expect_silent(edition_require(2025, what = "foo()"))

  local_edition(2024)
  expect_silent(edition_deprecate(2025, what = "foo()"))
  expect_snapshot(edition_require(2025, what = "foo()"), error = TRUE)
})

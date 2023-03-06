test_that("type argument is checked for proper input", {
  expect_snapshot_error(
    scale_colour_continuous(type = function() "abc")
  )
  expect_snapshot_error(
    scale_fill_continuous(type = geom_point)
  )
  expect_snapshot_error(
    scale_colour_binned(type = function(...) scale_colour_binned(aesthetics = c("fill", "point_colour")))
  )
  expect_snapshot_error(
    scale_fill_binned(type = scale_fill_brewer)
  )
  expect_snapshot_error(
    scale_fill_continuous(type = "abc")
  )
  expect_snapshot_error(
    scale_colour_continuous(type = "abc")
  )
})

test_that("palette with may_return_NA=FALSE works as expected", {
  sc <- scale_fill_continuous()
  # A palette that may return NAs, will have NAs replaced by the scale's na.value
  # by the scale:
  sc$palette <- structure(
    function(x) {
      rep(NA_character_, length(x))
    },
    may_return_NA = TRUE
  )
  sc$na.value <- "red"
  nat <- sc$map(0.5, limits = c(0, 1))
  expect_equal(nat, "red")

  # This palette is lying, because it returns NA even though it says it can't.
  # The scale will not replace the NA values, leading to further errors.
  # You should not do this in production, but it helps to test:
  sc <- scale_fill_continuous()
  sc$palette <- structure(
    function(x) {
      rep(NA_character_, length(x))
    },
    may_return_NA = FALSE
  )
  sc$na.value <- "red"
  nat <- sc$map(0.5, limits = c(0, 1))
  expect_equal(nat, NA_character_)
})

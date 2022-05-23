
test_that("expand_scale() produces a deprecation warning", {
  expect_warning(expand_scale(), "deprecated")
})

test_that("expansion() checks input", {
  expect_snapshot_error(expansion(mult = 2:4))
  expect_snapshot_error(expansion(add = 2:4))
})

# Expanding continuous scales -----------------------------------------

test_that("expand_limits_continuous() can override limits", {
  expect_identical(expand_limits_continuous(c(1, 2), coord_limits = c(NA, NA)), c(1, 2))
  expect_identical(expand_limits_continuous(c(1, 2), coord_limits = c(NA, 3)), c(1, 3))
  expect_identical(expand_limits_continuous(c(1, 2), coord_limits = c(0, NA)), c(0, 2))
})

test_that("expand_limits_continuous() expands limits", {
  expect_identical(expand_limits_continuous(c(1, 2), expand = expansion(add = 1)), c(0, 3))
})

test_that("expand_limits_continuous() expands coord-supplied limits", {
  expect_identical(
    expand_limits_continuous(c(1, 2), coord_limits = c(0, 4), expand = expansion(add = 1)),
    c(-1, 5)
  )
})

test_that("expand_limits_continuous_trans() expands limits in coordinate space", {
  limit_info <- expand_limits_continuous_trans(
    c(1, 2),
    expand = expansion(add = 0.5),
    trans = log10_trans()
  )

  expect_identical(
    limit_info$continuous_range,
    10^(expand_range4(log10(c(1, 2)), expansion(add = 0.5)))
  )

  expect_identical(
    limit_info$continuous_range_coord,
    expand_range4(log10(c(1, 2)), expansion(add = 0.5))
  )
})

test_that("introduced non-finite values fall back on scale limits", {
  limit_info <- expand_limits_continuous_trans(
    c(1, 100),
    expand = expansion(add = 2),
    trans = sqrt_trans()
  )

  expect_identical(limit_info$continuous_range, c(1, (sqrt(100) + 2)^2))
  expect_identical(limit_info$continuous_range_coord, c(-1, sqrt(100) + 2))
})

# Expanding discrete scales -----------------------------------------

test_that("expand_limits_discrete() can override limits with an empty range", {
  expect_identical(expand_limits_discrete(NULL, coord_limits = c(-1, 8)), c(-1, 8))
})

test_that("expand_limits_discrete() can override limits with a discrete range", {
  expect_identical(expand_limits_discrete(c("one", "two"), coord_limits = c(NA, NA)), c(1, 2))
  expect_identical(expand_limits_discrete(c("one", "two"), coord_limits = c(NA, 3)), c(1, 3))
  expect_identical(expand_limits_discrete(c("one", "two"), coord_limits = c(3, NA)), c(3, 2))
})

test_that("expand_limits_discrete() can override limits with a continuous range", {
  expect_identical(
    expand_limits_discrete(NULL, coord_limits = c(NA, NA), range_continuous = c(1, 2)),
    c(1, 2)
  )
  expect_identical(
    expand_limits_discrete(NULL, coord_limits = c(NA, 3), range_continuous = c(1, 2)),
    c(1, 3)
  )
  expect_identical(
    expand_limits_discrete(NULL, coord_limits = c(0, NA), range_continuous = c(1, 2)),
    c(0, 2)
  )
})

test_that("expand_limits_discrete() can override limits with a both discrete and continuous ranges", {
  expect_identical(
    expand_limits_discrete(c("one", "two"), coord_limits = c(NA, NA), range_continuous = c(1, 2)),
    c(1, 2)
  )
  expect_identical(
    expand_limits_discrete(c("one", "two"), coord_limits = c(NA, 3), range_continuous = c(1, 2)),
    c(1, 3)
  )
  expect_identical(
    expand_limits_discrete(c("one", "two"), coord_limits = c(0, NA), range_continuous = c(1, 2)),
    c(0, 2)
  )
})

test_that("expand_limits_continuous_trans() works with inverted transformations", {
  limit_info <- expand_limits_continuous_trans(
    c(1, 2),
    expand = expansion(add = 1),
    trans = reverse_trans()
  )

  expect_identical(limit_info$continuous_range, c(0, 3))
  expect_identical(limit_info$continuous_range_coord, c(0, -3))
})

test_that("expand_limits_scale_discrete() begrudgingly handles numeric limits", {
  expect_identical(
    expand_limits_discrete(
      -1:-16,
      coord_limits = c(NA, NA),
      range_continuous = c(-15, -2)
    ),
    c(-15, -2)
  )
})

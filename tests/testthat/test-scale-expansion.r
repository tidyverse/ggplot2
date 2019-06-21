
test_that("scale_dimension() can override scale limits", {
  scale <- scale_x_continuous(limits = c(1, 2))
  expect_identical(scale_dimension(scale, c(NA, NA)), c(1, 2))
  expect_identical(scale_dimension(scale, c(NA, 3)), c(1, 3))
  expect_identical(scale_dimension(scale, c(3, NA)), c(3, 2))
})

test_that("scale_dimension() returns limits in transformed scale space", {
  scale_log <- scale_x_log10(limits = c(1, 10))
  expect_identical(scale_dimension(scale_log), log10(c(1, 10)))

  # user-supplied limits are in data space
  expect_identical(scale_dimension(scale_log, coord_limits = c(1, 100)), log10(c(1, 100)))
})

test_that("scale_dimension() expands coord-supplied limits", {
  scale <- scale_x_continuous(limits = c(1, 2))
  expect_identical(
    scale_dimension(scale, coord_limits = c(0, 4), expansion = expand_scale(add = 1)),
    c(-1, 5)
  )
})

test_that("scale_dimension() expands limits", {
  scale <- scale_x_continuous(limits = c(1, 2))
  expect_identical(scale_dimension(scale, expansion = expand_scale(add = 1)), c(0, 3))

  # expansion takes place in coordinate space
  expect_identical(
    scale_dimension(scale, expansion = expand_scale(add = 0.5), coord_trans = log10_trans()),
    10^(expand_range4(log10(c(1, 2)), expand_scale(add = 0.5)))
  )
})

test_that("scale_dimension() coord_trans only affects expansion", {
  scale_log <- scale_x_log10(limits = c(1, 10))
  expect_identical(
    scale_dimension(scale_log, coord_trans = reverse_trans()),
    scale_dimension(scale_log)
  )
})

test_that("introduced non-finite values fall back on scale limits", {
  scale <- scale_x_continuous(limits = c(1, 100))
  expect_identical(
    scale_dimension(scale, expansion = expand_scale(add = 2), coord_trans = sqrt_trans()),
    c(1, (sqrt(100) + 2)^2)
  )
})

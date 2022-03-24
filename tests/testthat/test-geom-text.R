# compute_just ------------------------------------------------------------

test_that("vertical and horizontal positions are equivalent", {
  horiz <- compute_just(c("left", "middle", "right"), c(0, 0, 0))
  vert <- compute_just(c("bottom", "center", "top"), c(0, 0, 0))

  expect_equal(horiz, vert)
})

test_that("inward moves text towards center", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"), c(0, 0.5, 1)),
    c(0, 0.5, 1.0)
  )
})

test_that("outward moves text away from center", {
  expect_equal(
    compute_just(c("outward", "outward", "outward"), c(0, 0.5, 1)),
    c(1.0, 0.5, 0)
  )
})

test_that("inward points close to center are centered", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"), c(0.5 - 1e-3, 0.5, 0.5 + 1e-3)),
    c(0.5, 0.5, 0.5)
  )
})

test_that("inward moves text towards center at 90 degrees", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"),
                 c(0, 0.5, 1),
                 c(0, 0.5, 1),
                 c(90, 90, 90)),
    c(0, 0.5, 1.0)
  )
})

test_that("outward moves text away from center at 90 degrees", {
  expect_equal(
    compute_just(c("outward", "outward", "outward"),
                 c(0, 0, 0),
                 c(0, 0.5, 1),
                 c(90, 90, 90)),
    c(1.0, 0.5, 0)
  )
})

test_that("only inward and outward respond to angle", {
  expect_equal(
    compute_just(c("inward", "left", "outward"),
                 c(0, 0, 0),
                 c(0, 0.5, 1),
                 c(90, 90, 90)),
    c(0.0, 0.0, 0.0)
  )
})

test_that("inward moves text towards center at 150 degrees", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"),
                 c(0, 0.5, 1),
                 c(0, 0.5, 1),
                 c(150, 150, 150)),
    c(1.0, 0.5, 0.0)
  )
})

test_that("inward moves text towards center at -90 degrees", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"),
                 c(0, 0.5, 1),
                 c(0, 0.5, 1),
                 c(-90, -90, -90)),
    c(1.0, 0.5, 0.0)
  )
})

test_that("outward moves text away from center at 450 degrees", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"),
                 c(0, 0, 0),
                 c(0, 0.5, 1),
                 c(450, 450, 450)),
    c(0.0, 0.5, 1.0)
  )
})

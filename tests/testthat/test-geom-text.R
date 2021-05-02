context("geom_text")

# compute_just ------------------------------------------------------------

test_data <- tibble(hjust = c("left", "middle", "right"),
                    vjust = NA_real_,
                    x = c(0, 0, 0),
                    y = NA_real_)

test_that("hjust uses x", {
  expect_equal(
    compute_just(test_data, just_dir = "h"),
    c(0, 0.5, 1.0)
  )
})

test_data <- tibble(hjust = NA_real_,
                    vjust = c("bottom", "center", "top"),
                    x = NA_real_,
                    y = c(0, 0, 0))

test_that("vjust uses y", {
  expect_equal(
    compute_just(test_data, just_dir = "v"),
    c(0, 0.5, 1.0)
  )
})

test_data <- tibble(hjust = c("left", "middle", "right"),
                    vjust = c("bottom", "center", "top"),
                    x = c(0, 0, 0),
                    y = c(0, 0, 0))

test_that("vertical and horizontal positions are equivalent", {
  horiz <- compute_just(test_data, just_dir = "h")
  vert <- compute_just(test_data, just_dir = "v")

  expect_equal(horiz, vert)
})

test_data <- tibble(hjust = c("inward", "inward", "inward"),
                    vjust = c("inward", "inward", "inward"),
                    x = c(0, 0.5, 1),
                    y = c(0, 0.5, 1))

test_that("inward moves text towards middle", {
  expect_equal(
    compute_just(test_data, just_dir = "h"),
    c(0, 0.5, 1.0)
  )
})

test_that("inward moves text towards center", {
  expect_equal(
    compute_just(test_data, just_dir = "v"),
    c(0, 0.5, 1.0)
  )
})

test_data <- tibble(hjust = c("outward", "outward", "outward"),
                    vjust = c("outward", "outward", "outward"),
                    x = c(0, 0.5, 1),
                    y = c(0, 0.5, 1))

test_that("outward moves text away from middle", {
  expect_equal(
    compute_just(test_data, just_dir = "h"),
    c(1.0, 0.5, 0)
  )
})

test_that("outward moves text away from center", {
  expect_equal(
    compute_just(test_data, just_dir = "v"),
    c(1.0, 0.5, 0)
  )
})

test_data <- tibble(hjust = c("inward", "inward", "inward"),
                    vjust = c("inward", "inward", "inward"),
                    x = c(0.5 - 1e-3, 0.5, 0.5 + 1e-3),
                    y = c(0.5 - 1e-3, 0.5, 0.5 + 1e-3))

test_that("inward points close to middle are centered", {
  expect_equal(
    compute_just(test_data, just_dir = "h"),
    c(0.5, 0.5, 0.5)
  )
})

test_that("inward points close to center are centered", {
  expect_equal(
    compute_just(test_data, just_dir = "v"),
    c(0.5, 0.5, 0.5)
  )
})

test_data <- tibble(hjust = c("inward", "inward", "inward"),
                    vjust = NA_real_,
                    x = NA_real_,
                    y = c(0, 0.5, 1),
                    angle = 90)

test_that("hjust uses y if angle > 45", {
  expect_equal(
    compute_just(test_data, just_dir = "h"),
    c(0, 0.5, 1.0)
  )
})

test_data <- tibble(hjust = NA_real_,
                    vjust = c("inward", "inward", "inward"),
                    x = c(0, 0.5, 1),
                    y = NA_real_,
                    angle = 90)

test_that("vjust uses x if angle > 45", {
  expect_equal(
    compute_just(test_data, just_dir = "v"),
    c(0, 0.5, 1.0)
  )
})

test_data <- tibble(hjust = c("inward", "left", "outward"),
                    vjust = NA_real_,
                    x = NA_real_,
                    y = c(0, 0.5, 1),
                    angle = 90)

test_that("hjust mixed character", {
  expect_equal(
    compute_just(test_data, just_dir = "h"),
    c(0.0, 0.0, 0.0)
  )
})

test_data <- tibble(hjust = NA_real_,
                    vjust = c("inward", "left", "outward"),
                    x = c(0, 0.5, 1),
                    y = NA_real_,
                    angle = 90)

test_that("vjust mixed character", {
  expect_equal(
    compute_just(test_data, just_dir = "v"),
    c(0.0, 0.0, 0.0)
  )
})



test_that("standard alignment works", {
  df <- tibble::tribble(
    ~g, ~x, ~y,
    "a", 1, 2,
    "a", 3, 5,
    "a", 5, 1,
    "b", 2, 3,
    "b", 4, 6,
    "b", 6, 7
  )
  p <- ggplot(df, aes(x, y, fill = g)) + geom_area(color = "black")
  expect_doppelganger("align two areas", p)
})

test_that("alignment with cliffs works", {
  df <- tibble::tribble(
    ~g, ~x, ~y,
    "a", 1, 2,
    "a", 3, 5,
    "a", 5, 1,
    "b", 2, 3,
    "b", 4, 3,
    "b", 4, 6,
    "b", 6, 7
  )

  p <- ggplot(df, aes(x, y, fill = g)) + geom_area(color = "black")
  expect_doppelganger("align two areas with cliff", p)
})

test_that("alignment with negative and positive values works", {
  df <- tibble::tribble(
    ~g, ~x, ~y,
    "a", 1, 1,
    "a", 2, 4,
    "a", 3, -4,
    "a", 8, 0,
    "b", 2, 4,
    "b", 6, -4
  )

  p <- ggplot(df, aes(x, y, fill = g)) + geom_area(color = "black")
  expect_doppelganger("align two areas with pos/neg y", p)
})

test_that("alignment adjusts per panel", {
  # In particular, the adjustment (small offset used) should take panel-wise
  # data into account (#5227)

  df <- data_frame0(
    x = c(0, 1, 1000, 1001),
    y = c(-1, 1, -1, 1),
    g = c("A", "A", "B", "B")
  )
  p <- ggplot(df, aes(x, y))

  # Here, x-range is large, so adjustment should be larger
  ld <- layer_data(p + geom_area(aes(fill = g)))
  expect_equal(diff(ld$x[1:2]), 1/6, tolerance = 1e-4)

  # Here, x-ranges are smaller, so adjustment should be smaller instead of
  # considering the data as a whole
  ld <- layer_data(p + geom_area() + facet_wrap(vars(g), scales = "free_x"))
  expect_equal(diff(ld$x[1:2]), 1e-3, tolerance = 1e-4)

})

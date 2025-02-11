test_that("standard alignment works", {
  df <- data_frame0(
    g = rep(c("a", "b"), each = 3L),
    x = c(1, 3, 5, 2, 4, 6),
    y = c(2, 5, 1, 3, 6, 7)
  )
  p <- ggplot(df, aes(x, y, fill = g)) + geom_area(color = "black")
  expect_doppelganger("align two areas", p)
})

test_that("alignment with cliffs works", {
  df <- data_frame0(
    g = rep(c("a", "b"), 3:4),
    x = c(1, 3, 5, 2, 4, 4, 6),
    y = c(2, 5, 1, 3, 3, 6, 7)
  )
  p <- ggplot(df, aes(x, y, fill = g)) + geom_area(color = "black")
  expect_doppelganger("align two areas with cliff", p)
})

test_that("alignment with negative and positive values works", {
  df <- data_frame0(
    g = rep(c("a", "b"), c(4L, 2L)),
    x = c(1, 2, 3, 8, 2, 6),
    y = c(1, 4, -4, 0, 4, -4)
  )
  p <- ggplot(df, aes(x, y, fill = g)) + geom_area(color = "black")
  expect_doppelganger("align two areas with pos/neg y", p)
})

test_that("alignment adjusts per panel", {
  # In particular, the adjustment (small offset used) should take panel-wise
  # data into account (#5227)

  df <- data_frame0(
    x = c(0, 1, 1000, 1001, 0, 1, 1000, 1001),
    y = c(-1, 1, -1, 1, -1, 1, -1, 1),
    f = c("A", "A", "B", "B", "A", "A", "B", "B"),
    g = c("a", "a", "b", "b", "c", "c", "d", "d")
  )
  p <- ggplot(df, aes(x, y, group = g))

  # Here, x-range is large, so adjustment should be larger
  ld <- get_layer_data(p + geom_area(aes(fill = f)))
  expect_equal(diff(ld$x[1:2]), 1/6, tolerance = 1e-4)

  # Here, x-ranges are smaller, so adjustment should be smaller instead of
  # considering the data as a whole
  ld <- get_layer_data(p + geom_area() + facet_wrap(vars(f), scales = "free_x"))
  expect_equal(diff(ld$x[1:2]), 1e-3, tolerance = 1e-4)

})

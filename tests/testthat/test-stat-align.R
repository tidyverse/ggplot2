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

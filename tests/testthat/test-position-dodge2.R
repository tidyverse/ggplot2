context("position_dodge2")

test_that("find_x_overlaps identifies overlapping groups", {

  df1 <- data.frame(
    xmin = c(1, 3, 6, 11, 13),
    xmax = c(5, 7, 9, 15, 16)
  )

  df2 <- data.frame(
    xmin = c(0.85, 0.80, 1.90, 1.90, 2.80),
    xmax = c(1.15, 1.20, 2.10, 2.05, 3.20)
  )

  expect_equal(find_x_overlaps(df1), c(1, 1, 1, 2, 2))
  expect_equal(find_x_overlaps(df2), c(1, 1, 2, 2, 3))
})

test_that("single element is rescaled based on n", {
  df <- data.frame(xmin = 1, xmax = 2)
  out <- pos_dodge2(df, n = 2)
  expect_equal(out$xmax - out$xmin, 0.5)
})

test_that("rectangles are dodged", {
  df <- data.frame(
    xmin = c(1, 3, 6, 11, 13),
    xmax = c(5, 7, 9, 15, 16),
    ymin = c(1, 1, 5, 2, 2),
    ymax = c(3, 4, 8, 6, 7),
    fill = c("a", "b", "c", "a", "b")
  )

  p <- ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    geom_rect(aes(fill = fill), position = "dodge2", alpha = 0.8)

  expect_false(any(duplicated(find_x_overlaps(layer_data(p)))))
})

test_that("cols at the same x position are dodged", {
  df <- data.frame(
    x = c("a", "a", "b"),
    n = c(1, 5, 10),
    stringsAsFactors = FALSE
  )

  p <- ggplot(df, aes(1, n, fill = x)) +
    geom_col(position = "dodge2", alpha = 0.5)

  expect_false(any(duplicated(find_x_overlaps(layer_data(p)))))
})

test_that("padding argument controls space between elements", {
  p1 <- ggplot(iris, aes(1, Sepal.Length, fill = Sepal.Width < 3.2)) +
    geom_boxplot(position = position_dodge2(padding = 0))
  p2 <- ggplot(iris, aes(1, Sepal.Length, fill = Sepal.Width < 3.2)) +
    geom_boxplot(position = position_dodge2(padding = 0.1))

  d1 <- layer_data(p1)
  d2 <- layer_data(p2)

  gaps <- function(df) {
    gap <- vector()
    for (i in 2:nrow(df)) {
      gap[i - 1] <- df$xmin[i] - df$xmax[i - 1]
    }
    gap
  }

  expect_equal(gaps(d1), 0)
  expect_equal(gaps(d2), 0.0375)
})

test_that("boxes in facetted plots keep the correct width", {

  p <- ggplot(mtcars, aes(x = factor(vs), y = mpg)) +
    facet_wrap( ~ factor(cyl)) +
    geom_boxplot()

  d <- layer_data(p)

  expect_true(all(d$xmax - d$xmin == 0.75))
})

test_that("width of groups is computed per facet", {
  df <- tibble::tribble(
    ~g1, ~g2,  ~y,
    "x", "a",  1,
    "x", "b",  2,
    "y", "a",  3,
    "y", "b",  4,
    "y", "c",  3,
  )

  p <- ggplot(df, aes("x", y, fill = g2)) +
    geom_col(position = position_dodge2(preserve = "single")) +
    facet_wrap(vars(g1))

  d <- layer_data(p)
  width <- d$xmax - d$xmin

  expect_true(all(width == (0.9 / 3) * 0.9))
})

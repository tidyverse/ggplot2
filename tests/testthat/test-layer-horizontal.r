
context("Horizontal Layer")

expect_identically_built <- function(object, expected, flipped = NULL) {
  sort <- function(x) x[order(names(x))]
  flipped <- function(fun, flip_fun = ggplot2:::flip_aes) {
    function(x, ...) fun(flip_fun(x), ...)
  }

  h <- ggplot2:::ggplot_build(expected)
  v <- ggplot2:::ggplot_build(object)

  h_data <- lapply(h$data, flipped(sort))
  v_data <- lapply(v$data, sort)
  expect_identical(h_data, v_data)

  ## h_ranges <- lapply(h$panel$ranges, flipped(sort, ggplot2:::flip_labels))
  ## v_ranges <- lapply(v$panel$ranges, sort)
  ## expect_identical(h_ranges, v_ranges)


  if (FALSE && !is.null(flipped)) {
    f <- ggplot2:::ggplot_build(flipped)

    f_data <- lapply(f$data, sort)
    expect_identical(h_data, f_data)
  }

}


test_that("geom_boxplot() flips correctly", {
  v <- ggplot(mpg, aes(class, hwy))+ geom_boxplot()
  h <- ggplot(mpg, aes(hwy, class)) + geom_boxplot(orient = "h")
  f <- ggplot(mpg, aes(class, hwy)) + geom_boxplot() + coord_flip()

  expect_identically_built(h, v, f)
})

test_that("geom_histogram() flips correctly", {
  v <- ggplot(mpg, aes(class, hwy))+ geom_boxplot()
  h <- ggplot(mpg, aes(hwy, class)) + geom_boxplot(orient = "h")

  expect_identically_built(h, v)
})


test_that("geom_violin() flips correctly", {
  v <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin()
  h <- ggplot(mtcars, aes(mpg, factor(cyl))) + geom_violin(orient = "h")

  expect_identically_built(h, v)
})

test_that("dodged geom_linerange() and geom_bar() flip correctly", {
  df <- data.frame(
    x = factor(c(1, 1, 2, 2)),
    y = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)
  )
  dodge <- position_dodge(width=0.9)

  v <- ggplot(df, aes(x, y, fill = group)) +
    geom_bar(position = dodge, stat = "identity") +
    geom_linerange(aes(ymin = lower, ymax = upper))
  h <- ggplot(df, aes(y, x, fill = group)) +
    geom_bar(position = dodge, stat = "identity", orient = "h") +
    geom_linerange(aes(xmin = lower, xmax = upper, orient = "h"))

  expect_identically_built(h, v)
})

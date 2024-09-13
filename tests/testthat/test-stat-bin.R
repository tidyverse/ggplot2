test_that("stat_bin throws error when wrong combination of aesthetic is present", {
  dat <- data_frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_snapshot_error(ggplot_build(ggplot(dat) + stat_bin()))

  expect_snapshot_error(ggplot_build(ggplot(dat, aes(x, y)) + stat_bin()))

  expect_snapshot_error(ggplot_build(ggplot(dat, aes(x)) + stat_bin(y = 5)))
})

test_that("stat_bin works in both directions", {
  p <- ggplot(mpg, aes(hwy)) + stat_bin(bins = 30)
  x <- get_layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(mpg, aes(y = hwy)) + stat_bin(bins = 30)
  y <- get_layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, flip_data(y, TRUE)[,names(x)])
})

test_that("bins specifies the number of bins", {
  df <- data_frame(x = 1:10)
  out <- function(x, ...) {
    get_layer_data(ggplot(df, aes(x)) + geom_histogram(...))
  }

  expect_equal(nrow(out(bins = 2)), 2)
  expect_equal(nrow(out(bins = 100)), 100)
})

test_that("binwidth computes widths for function input", {
  df <- data_frame(x = 1:100)
  out <- get_layer_data(ggplot(df, aes(x)) + geom_histogram(binwidth = function(x) 5))

  expect_equal(nrow(out), 21)
})

test_that("geom_histogram defaults to pad = FALSE", {
  df <- data_frame(x = 1:3)
  out <- get_layer_data(ggplot(df, aes(x)) + geom_histogram(binwidth = 1))

  expect_equal(out$count, c(1, 1, 1))
})

test_that("geom_freqpoly defaults to pad = TRUE", {
  df <- data_frame(x = 1:3)
  out <- get_layer_data(ggplot(df, aes(x)) + geom_freqpoly(binwidth = 1))

  expect_equal(out$count, c(0, 1, 1, 1, 0))
})

test_that("can use breaks argument", {
  df <- data_frame(x = 1:3)
  out <- get_layer_data(ggplot(df, aes(x)) + geom_histogram(breaks = c(0, 1.5, 5)))

  expect_equal(out$count, c(1, 2))
})

test_that("breaks computes bin boundaries for function input", {
  df <- data.frame(x = c(0, 0, 0, 1:3))
  out <- layer_data(ggplot(df, aes(x)) +
                      geom_histogram(breaks = function(x) c(0, 0.5, 2.5, 7.5)))

  expect_equal(out$count, c(3, 2, 1))
})

test_that("fuzzy breaks are used when cutting", {
  df <- data_frame(x = c(-1, -0.5, -0.4, 0))
  p <- ggplot(df, aes(x)) +
    geom_histogram(binwidth = 0.1, boundary = 0.1, closed = "left")

  bins <- get_layer_data(p) %>% subset(count > 0) %>% .[1:5]
  expect_equal(bins$count, c(1, 1, 1, 1))
})

test_that("breaks are transformed by the scale", {
   df <- data_frame(x = rep(1:4, 1:4))
   base <- ggplot(df, aes(x)) + geom_histogram(breaks = c(1, 2.5, 4))

   out1 <- get_layer_data(base)
   out2 <- get_layer_data(base + scale_x_sqrt())
   expect_equal(out1$xmin, c(1, 2.5))
   expect_equal(out2$xmin, sqrt(c(1, 2.5)))
})

test_that("geom_histogram() can be drawn over a 0-width range (#3043)", {
  df <- data_frame(x = rep(1, 100))
  out <- get_layer_data(ggplot(df, aes(x)) + geom_histogram(bins = 30))

  expect_equal(nrow(out), 1)
  expect_equal(out$xmin, 0.95)
  expect_equal(out$xmax, 1.05)
})

test_that("stat_bin() provides width (#3522)", {
  binwidth <- 1.03
  df <- data_frame(x = 1:10)
  p <- ggplot(df) +
    stat_bin(
      aes(
        x,
        xmin = after_stat(x - width / 2),
        xmax = after_stat(x + width / 2),
        ymin = after_stat(0),
        ymax = after_stat(count)
      ),
      geom = "rect",
      binwidth = binwidth
    )
  out <- get_layer_data(p)

  expect_equal(nrow(out), 10)
  # (x + width / 2) - (x - width / 2) = width
  expect_equal(out$xmax - out$xmin, rep(binwidth, 10))
})

# Underlying binning algorithm --------------------------------------------

test_that("bins() computes fuzz with non-finite breaks", {
  test <- bins(breaks = c(-Inf, 1, Inf))
  expect_equal(test$fuzzy, test$breaks, tolerance = 1e-10)
  difference <- test$fuzzy - test$breaks
  expect_equal(difference[2], 1000 * .Machine$double.eps, tolerance = 0)
})

test_that("bins is strictly adhered to", {

  nbins <- c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50)

  # Default case
  nbreaks <- vapply(nbins, function(bins) {
    length(bin_breaks_bins(c(0, 10), bins)$breaks)
  }, numeric(1))
  expect_equal(nbreaks, nbins + 1)

  # Center is provided
  nbreaks <- vapply(nbins, function(bins) {
    length(bin_breaks_bins(c(0, 10), bins, center = 0)$breaks)
  }, numeric(1))
  expect_equal(nbreaks, nbins + 1)

  # Boundary is provided
  nbreaks <- vapply(nbins, function(bins) {
    length(bin_breaks_bins(c(0, 10), bins, boundary = 0)$breaks)
  }, numeric(1))
  expect_equal(nbreaks, nbins + 1)

})

comp_bin <- function(df, ...) {
  plot <- ggplot(df, aes(x = x)) + stat_bin(...)
  get_layer_data(plot)
}

test_that("inputs to binning are checked", {
  dat <- data_frame(x = c(0, 10))
  expect_snapshot_error(comp_bin(dat, breaks = letters))
  expect_snapshot_error(bin_breaks_width(3))
  expect_snapshot_error(comp_bin(dat, binwidth = letters))
  expect_snapshot_error(comp_bin(dat, binwidth = -4))

  expect_snapshot_error(bin_breaks_bins(3))
  expect_snapshot_error(comp_bin(dat, bins = -4))
})

test_that("closed left or right", {
  dat <- data_frame(x = c(0, 10))

  res <- comp_bin(dat, binwidth = 10, pad = FALSE)
  expect_identical(res$count, c(1, 1))
  res <- comp_bin(dat, binwidth = 10, boundary = 5, pad = FALSE)
  expect_identical(res$count, c(1, 1))
  res <- comp_bin(dat, binwidth = 10, boundary = 0, pad = FALSE)
  expect_identical(res$count, 2)
  res <- comp_bin(dat, binwidth = 5, boundary = 0, pad = FALSE)
  expect_identical(res$count, c(1, 1))

  res <- comp_bin(dat, binwidth = 10, pad = FALSE, closed = "left")
  expect_identical(res$count, c(1, 1))
  res <- comp_bin(dat, binwidth = 10, boundary = 5, pad = FALSE, closed = "left")
  expect_identical(res$count, c(1, 1))
  res <- comp_bin(dat, binwidth = 10, boundary = 0, pad = FALSE, closed = "left")
  expect_identical(res$count, c(2))
  res <- comp_bin(dat, binwidth = 5, boundary = 0, pad = FALSE, closed = "left")
  expect_identical(res$count, c(1, 1))
})

test_that("setting boundary and center", {
  # numeric
  df <- data_frame(x = c(0, 30))

  # Error if both boundary and center are specified
  expect_error(comp_bin(df, boundary = 5, center = 0), "one of `boundary` and `center`")

  res <- comp_bin(df, binwidth = 10, boundary = 0, pad = FALSE)
  expect_identical(res$count, c(1, 0, 1))
  expect_identical(res$xmin[1], 0)
  expect_identical(res$xmax[3], 30)

  res <- comp_bin(df, binwidth = 10, center = 0, pad = FALSE)
  expect_identical(res$count, c(1, 0, 0, 1))
  expect_identical(res$xmin[1], df$x[1] - 5)
  expect_identical(res$xmax[4], df$x[2] + 5)
})

test_that("weights are added", {
  df <- data_frame(x = 1:10, y = 1:10)
  p <- ggplot(df, aes(x = x, weight = y)) + geom_histogram(binwidth = 1)
  out <- get_layer_data(p)

  expect_equal(out$count, df$y)
})

test_that("bin errors at high bin counts", {
  expect_error(bin_breaks_width(c(1, 2e6), 1), "The number of histogram bins")
})

# stat_count --------------------------------------------------------------

test_that("stat_count throws error when both x and y aesthetic present", {
  dat <- data_frame(x = c("a", "b", "c"), y = c(1, 5, 10))

  expect_snapshot_error(ggplot_build(ggplot(dat, aes(x, y)) + stat_count()))
})

test_that("stat_count preserves x order for continuous and discrete", {
  # x is numeric
  b <- ggplot_build(ggplot(mtcars, aes(carb)) + geom_bar())
  expect_identical(b$data[[1]]$x, c(1,2,3,4,6,8))
  expect_identical(b$data[[1]]$y, c(7,10,3,10,1,1))

  # x is factor where levels match numeric order
  mtcars$carb2 <- factor(mtcars$carb)
  b <- ggplot_build(ggplot(mtcars, aes(carb2)) + geom_bar())
  expect_identical(b$data[[1]]$x, mapped_discrete(1:6))
  expect_identical(b$data[[1]]$y, c(7,10,3,10,1,1))

  # x is factor levels differ from numeric order
  mtcars$carb3 <- factor(mtcars$carb, levels = c(4,1,2,3,6,8))
  b <- ggplot_build(ggplot(mtcars, aes(carb3)) + geom_bar())
  expect_identical(b$data[[1]]$x, mapped_discrete(1:6))
  expect_identical(b$layout$panel_params[[1]]$x$get_labels(), c("4","1","2","3","6","8"))
  expect_identical(b$data[[1]]$y, c(10,7,10,3,1,1))
})

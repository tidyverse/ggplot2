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
    length(compute_bins(c(0, 10), bins = bins)$breaks)
  }, numeric(1))
  expect_equal(nbreaks, nbins + 1)

  # Center is provided
  nbreaks <- vapply(nbins, function(bins) {
    length(compute_bins(c(0, 10), bins = bins, center = 0)$breaks)
  }, numeric(1))
  expect_equal(nbreaks, nbins + 1)

  # Boundary is provided
  nbreaks <- vapply(nbins, function(bins) {
    length(compute_bins(c(0, 10), bins = bins, boundary = 0)$breaks)
  }, numeric(1))
  expect_equal(nbreaks, nbins + 1)

})

comp_bin <- function(df, ...) {
  plot <- ggplot(df, aes(x = x)) + stat_bin(...)
  get_layer_data(plot)
}

test_that("inputs to binning are checked", {
  dat <- data_frame(x = c(0, 10))
  expect_snapshot_error(compute_bins(dat, breaks = letters))
  expect_snapshot_error(compute_bins(dat, binwidth = letters))
  expect_snapshot_error(compute_bins(dat, binwidth = -4))
  expect_snapshot_error(compute_bins(dat, bins = -4))
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
  expect_snapshot_warning(comp_bin(df, boundary = 5, center = 0, bins = 30))

  res <- comp_bin(df, binwidth = 10, boundary = 0, pad = FALSE)
  expect_identical(res$count, c(1, 0, 1))
  expect_identical(res$xmin[1], 0)
  expect_identical(res$xmax[3], 30)

  res <- comp_bin(df, binwidth = 10, center = 0, boundary = NULL, pad = FALSE)
  expect_identical(res$count, c(1, 0, 0, 1))
  expect_identical(res$xmin[1], df$x[1] - 5)
  expect_identical(res$xmax[4], df$x[2] + 5)
})


test_that("bin errors at high bin counts", {
  expect_snapshot(compute_bins(c(1, 2e6), binwidth =  1), error = TRUE)
})

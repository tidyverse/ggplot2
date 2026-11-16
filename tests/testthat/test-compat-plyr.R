test_that("dapply() splits, applies and reassembles", {
  df <- data_frame0(
    g = c("a", "a", "b", "b"),
    x = c(1, 2, 3, 4)
  )
  out <- dapply(df, "g", function(d) data_frame0(n = nrow(d), s = sum(d$x)))
  expect_identical(out$g, c("a", "b"))
  expect_identical(out$n, c(2L, 2L))
  expect_identical(out$s, c(3, 7))
})

test_that("dapply() returns an empty data frame for empty input", {
  # zero rows means zero groups: `fun()` must never be called, and splitting
  # must not be attempted (`max(integer(0))` is `-Inf`, which `seq_len()` rejects)
  df <- data_frame0(g = character(0), x = numeric(0))

  called <- FALSE
  out <- dapply(df, "g", function(d) {
    called <<- TRUE
    data_frame0(n = nrow(d))
  })

  expect_false(called)
  expect_equal(nrow(out), 0)
})

test_that("dapply() short-circuits single groups but not empty ones", {
  # a single *non-empty* group skips the split entirely ...
  df <- data_frame0(g = factor(c("1", "1"), levels = "1"), x = c(1, 2))
  out <- dapply(df, "g", function(d) data_frame0(n = nrow(d)))
  expect_equal(out$n, 2)

  # ... but an empty `df` has no groups, even when the grouping column has
  # exactly one level. Returning early also avoids the phantom row with
  # `PANEL = NA` that `apply_fun()` would otherwise produce.
  df0 <- data_frame0(g = factor(character(0), levels = "1"), x = numeric(0))
  out0 <- dapply(df0, "g", function(d) data_frame0(n = nrow(d)))
  expect_equal(nrow(out0), 0)
})

test_that("a layer whose data is fully censored does not abort the build", {
  # https://github.com/YuLab-SMU/ggtree/issues/658
  d1 <- data_frame0(panel = "a", x = c(1, 2), g = c("A", "B"))
  d2 <- data_frame0(panel = "b", x = c(10, 20), g = c("A", "B"))

  p <- ggplot() +
    geom_boxplot(data = d1, aes(x = x, group = g)) +
    geom_boxplot(data = d2, aes(x = x, group = g)) +
    facet_wrap(~panel) +
    xlim(0, 5)

  # `xlim()` drops every row of the second layer, leaving it with zero rows
  expect_warning(ggplot_build(p), "non-finite")
  expect_silent(ggplotGrob(suppressWarnings(ggplot_build(p))))
})

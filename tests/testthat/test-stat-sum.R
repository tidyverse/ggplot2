context("stat_sum")

test_that("handles grouping correctly", {
  d <- diamonds[1:1000, ]
  all_ones <- function(x) all.equal(mean(x), 1)

  base <- ggplot(d, aes(cut, clarity))

  ret <- pdata(base + stat_sum())[[1]]
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(ret$prop))

  ret <- pdata(base + stat_sum(aes(group = 1)))[[1]]
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_equal(sum(ret$prop), 1)

  ret <- pdata(base + stat_sum(aes(group = cut)))[[1]]
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$x, FUN = sum)))

  ret <- pdata(base + stat_sum(aes(group = cut, colour = cut)))[[1]]
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$x, FUN = sum)))

  ret <- pdata(base + stat_sum(aes(group = clarity)))[[1]]
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$y, FUN = sum)))

  ret <- pdata(base + stat_sum(aes(group = clarity, colour = cut)))[[1]]
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all_ones(tapply(ret$prop, ret$y, FUN = sum)))

  ret <- pdata(base + stat_sum(aes(group = 1, weight = price)))[[1]]
  expect_equal(nrow(ret), 38)
  expect_equal(sum(ret$n), sum(d$price))
  expect_equal(sum(ret$prop), 1)
})

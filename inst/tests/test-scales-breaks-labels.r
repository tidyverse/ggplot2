context("Scales: breaks and labels")

test_that("labels match breaks, even when outside limits", {
  sc <- scale_y_continuous(breaks=1:4, labels=1:4, limits = c(1, 3))
  
  expect_equal(scale_breaks(sc), c(1:3, NA))
  expect_equal(scale_labels(sc), 1:4)
  expect_equal(scale_breaks_minor(sc), c(1, 1.5, 2, 2.5, 3))
})


test_that("labels must match breaks", {
  expect_that(scale_x_discrete(breaks = 1:3, labels = 1:2),
    throws_error("unequal lengths"))
  expect_that(scale_x_continuous(breaks = 1:3, labels = 1:2),
    throws_error("unequal lengths"))
})

test_that("labels don't have extra spaces", {
  labels <- c("a", "abc", "abcdef")

  sc1 <- scale_x_discrete(limits = labels)
  sc2 <- scale_fill_discrete(limits = labels)
  
  expect_equal(scale_labels(sc1), labels)
  expect_equal(scale_labels(sc2), labels)
  
})

test_that("no minor breaks when only one break", {
  sc1 <- scale_x_discrete(limits = "a")
  sc2 <- scale_x_continuous(limits = 1)
  
  expect_equal(length(scale_breaks_minor(sc1)), 0)
  expect_equal(length(scale_breaks_minor(sc2)), 0)
  
})

init_scale <- function(...) {
  sc <- scale_x_discrete(...)
  scale_train(sc, factor(1:100))
  expect_that(length(scale_limits(sc)), equals(100))
  sc
}

test_that("discrete labels match breaks", {
  
  sc <- init_scale(breaks = 0:5 * 10)
  expect_equal(length(scale_breaks(sc)), 5)
  expect_equal(length(scale_labels(sc)), 5)
  expect_equivalent(scale_labels(sc), scale_breaks(sc))

  sc <- init_scale(breaks = 0:5 * 10, labels = letters[1:6])
  expect_equal(length(scale_breaks(sc)), 5)
  expect_equal(length(scale_labels(sc)), 5)
  expect_equal(scale_labels(sc), letters[2:6])

  sc <- init_scale(breaks = 0:5 * 10, labels = 
    function(x) paste(x, "-", sep = ""))
  expect_equal(scale_labels(sc), c("10-", "20-", "30-", "40-", "50-"))
  
  pick_5 <- function(x) sample(x, 5)
  sc <- init_scale(breaks = pick_5)
  expect_equal(length(scale_breaks(sc)), 5)
  expect_equal(length(scale_labels(sc)), 5)
  
})


test_that("scale breaks with numeric log transformation", {
  sc <- scale_x_continuous(limits = c(1, 1e5), trans = log10_trans())
  expect_equal(scale_breaks(sc), c(0, 2, 4)) # 1, 100, 10000
  expect_equal(scale_breaks_minor(sc), c(0, 1, 2, 3, 4, 5))
})

test_that("continuous scales with no data have no breaks or labels", {
  sc <- scale_x_continuous()
  
  expect_equal(scale_breaks(sc), numeric())
  expect_equal(scale_labels(sc), character())
  expect_equal(scale_limits(sc), c(0, 1))
  
})

test_that("discrete scales with no data have no breaks or labels", {
  sc <- scale_x_discrete()
  
  expect_equal(scale_breaks(sc), numeric())
  expect_equal(scale_labels(sc), character())
  expect_equal(scale_limits(sc), c(0, 1))
})

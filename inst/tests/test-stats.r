context("Stats")

test_that("plot succeeds even if some computation fails", {
  p1 <- ggplot(mtcars, aes(disp, mpg)) + 
    geom_point() + 
    facet_grid(gear ~ carb)
  p2 <- p1 + geom_smooth()
  
  b1 <- ggplot_build(p1)
  expect_equal(length(b1$data), 1)
  
  expect_warning(b2 <- ggplot_build(p2))
  expect_equal(length(b2$data), 2)
  
})

# helper function for stat calc tests.
test_stat <- function(stat) {
  stat$data <- transform(stat$data, PANEL = 1)
  dat <- stat$compute_aesthetics(stat$data, ggplot())
  dat <- add_group(dat)
  stat$calc_statistic(dat, NULL)
}

context("stat-sum")

test_that("stat_sum", {
  d <- diamonds[1:1000, ]

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all(ret$prop == 1))
  
  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = 1), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), nrow(d))
  expect_equal(sum(ret$prop), 1)
  
  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = cut), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all(ddply(ret, .(x), summarize, prop = sum(prop))$prop == 1))

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = cut, colour = cut), data =  d))
  expect_equal(dim(ret), c(38, 6))
  expect_equal(ret$x, ret$colour)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all(ddply(ret, .(x), summarize, prop = sum(prop))$prop == 1))
  
  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = clarity), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all(ddply(ret, .(y), summarize, prop = sum(prop))$prop == 1))

  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = clarity, colour = cut), data =  d))
  expect_equal(dim(ret), c(38, 6))
  expect_equal(ret$x, ret$colour)
  expect_equal(sum(ret$n), nrow(d))
  expect_true(all(ddply(ret, .(y), summarize, prop = sum(prop))$prop == 1))
  
  ret <- test_stat(stat_sum(aes(x = cut, y = clarity, group = 1, weight = price), data =  d))
  expect_equal(dim(ret), c(38, 5))
  expect_equal(sum(ret$n), sum(d$price))
  expect_equal(sum(ret$prop), 1)
})

# helper function for stat calc tests.
test_stat_scale <- function(stat, scale) {
  stat$data <- transform(stat$data, PANEL = 1)
  dat <- stat$compute_aesthetics(stat$data, ggplot())
  dat <- add_group(dat)
  stat$calc_statistic(dat, scale)
}

context("stat-bin2d")

test_that("stat-bin2d", {
  d <- diamonds[1:1000,]

  full_scales <- list(x = scale_x_continuous(limits = range(d$carat, na.rm=TRUE)),
                      y = scale_y_continuous(limits = range(d$depth, na.rm=TRUE)))
  ret <- test_stat_scale(stat_bin2d(aes(x = carat, y = depth), data=d), full_scales)
  expect_equal(dim(ret), c(191,12))

  d$carat[1] <- NA
  d$depth[2] <- NA

  full_scales <- list(x = scale_x_continuous(limits = range(d$carat, na.rm=TRUE)),
                      y = scale_y_continuous(limits = range(d$depth, na.rm=TRUE)))
  ret <- test_stat_scale(stat_bin2d(aes(x = carat, y = depth), data=d), full_scales)
  expect_equal(dim(ret), c(191,12))
})

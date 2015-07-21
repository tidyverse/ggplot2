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


test_that("out-of-range breaks are dropped", {
  # Limits are explicitly specified, automatic labels
  sc <- scale_x_continuous(breaks=1:5, limits = c(2, 4))
  bi <- scale_break_info(sc)
  expect_equal(bi$labels, as.character(2:4))
  expect_equal(bi$major, c(0, 0.5, 1))
  expect_equal(bi$major_source, 2:4)


  # Limits and labels are explicitly specified
  sc <- scale_x_continuous(breaks=1:5, labels=letters[1:5], limits = c(2, 4))
  bi <- scale_break_info(sc)
  expect_equal(bi$labels, letters[2:4])
  expect_equal(bi$major, c(0, 0.5, 1))
  expect_equal(bi$major_source, 2:4)


  # Limits are specified, and all breaks are out of range
  sc <- scale_x_continuous(breaks=c(1,5), labels=letters[c(1,5)], limits = c(2, 4))
  bi <- scale_break_info(sc)
  expect_equal(length(bi$labels), 0)
  expect_equal(length(bi$major), 0)
  expect_equal(length(bi$major_source), 0)


  # limits aren't specified, automatic labels
  # limits are set by the data
  sc <- scale_x_continuous(breaks=1:5)
  scale_train_df(sc, data.frame(x=2:4))
  bi <- scale_break_info(sc)
  expect_equal(bi$labels, as.character(2:4))
  expect_equal(bi$major_source, 2:4)
  expect_equal(bi$major, c(0, 0.5, 1))


  # Limits and labels are specified
  sc <- scale_x_continuous(breaks=1:5, labels=letters[1:5])
  scale_train_df(sc, data.frame(x=2:4))
  bi <- scale_break_info(sc)
  expect_equal(bi$labels, letters[2:4])
  expect_equal(bi$major_source, 2:4)
  expect_equal(bi$major, c(0, 0.5, 1))


  # Limits aren't specified, and all breaks are out of range of data
  sc <- scale_x_continuous(breaks=c(1,5), labels=letters[c(1,5)])
  scale_train_df(sc, data.frame(x=2:4))
  bi <- scale_break_info(sc)
  expect_equal(length(bi$labels), 0)
  expect_equal(length(bi$major), 0)
  expect_equal(length(bi$major_source), 0)
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

test_that("suppressing breaks, minor_breask, and labels", {
  expect_equal(scale_breaks(scale_x_continuous(breaks = NULL, limits = c(1, 3))), NULL)
  expect_equal(scale_breaks(scale_x_discrete(breaks = NULL, limits = c(1, 3))), NULL)
  expect_equal(scale_breaks_minor(scale_x_continuous(minor_breaks = NULL, limits = c(1, 3))), NULL)

  expect_equal(scale_labels(scale_x_continuous(labels = NULL, limits = c(1, 3))), NULL)
  expect_equal(scale_labels(scale_x_discrete(labels = NULL, limits = c(1, 3))), NULL)

  # date, datetime
  lims <- as.Date(c("2000/1/1", "2000/2/1"))
  expect_equal(scale_breaks(scale_x_date(breaks = NULL, limits = lims)), NULL)
  # NA is defunct, should throw error
  expect_error(scale_breaks(scale_x_date(breaks = NA, limits = lims)))
  expect_equal(scale_labels(scale_x_date(labels = NULL, limits = lims)), NULL)
  expect_error(scale_labels(scale_x_date(labels = NA, limits = lims)))
  expect_equal(scale_breaks_minor(scale_x_date(minor_breaks= NULL, limits = lims)), NULL)
  expect_error(scale_breaks_minor(scale_x_date(minor_breaks = NA, limits = lims)))

  # date, datetime
  lims <- as.POSIXct(c("2000/1/1 0:0:0", "2010/1/1 0:0:0"))
  expect_equal(scale_breaks(scale_x_datetime(breaks = NULL, limits = lims)), NULL)
  expect_error(scale_breaks(scale_x_datetime(breaks = NA, limits = lims)))
  expect_equal(scale_labels(scale_x_datetime(labels = NULL, limits = lims)), NULL)
  expect_error(scale_labels(scale_x_datetime(labels = NA, limits = lims)))
  expect_equal(scale_breaks_minor(scale_x_datetime(minor_breaks= NULL, limits = lims)), NULL)
  expect_error(scale_breaks_minor(scale_x_datetime(minor_breaks= NA, limits = lims)))

})

test_that("scale_breaks with explicit NA options (deprecated)", {
  # NA is defunct, should throw error

  # X
  sxc <- scale_x_continuous(breaks=NA)
  scale_train(sxc, 1:3)
  expect_error(scale_breaks(sxc))
  expect_error(scale_breaks_minor(sxc))

  # Y
  syc <- scale_y_continuous(breaks=NA)
  scale_train(syc, 1:3)
  expect_error(scale_breaks(syc))
  expect_error(scale_breaks_minor(syc))

  # Alpha
  sac <- scale_alpha_continuous(breaks=NA)
  scale_train(sac,1:3)
  expect_error(scale_breaks(sac))

  # Size
  ssc <- scale_size_continuous(breaks=NA)
  scale_train(ssc,1:3)
  expect_error(scale_breaks(ssc))

  # Fill
  sfc <- scale_fill_continuous(breaks=NA)
  scale_train(sfc,1:3)
  expect_error(scale_breaks(sfc))

  # Colour
  scc <- scale_colour_continuous(breaks=NA)
  scale_train(scc,1:3)
  expect_error(scale_breaks(scc))

})


test_that("breaks can be specified by names of labels", {
  labels <- setNames(LETTERS[1:4], letters[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = labels)
  expect_equal(as.vector(scale_breaks(s)), letters[1:4])
  expect_equal(as.vector(scale_labels(s)), LETTERS[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = rev(labels))
  expect_equal(as.vector(scale_breaks(s)), letters[1:4])
  expect_equal(as.vector(scale_labels(s)), LETTERS[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = labels[1:2])
  expect_equal(as.vector(scale_breaks(s)), letters[1:4])
  expect_equal(as.vector(scale_labels(s)), c("A", "B", "c", "d"))

  s <- scale_x_discrete(limits = letters[1:4], labels = labels[3:4])
  expect_equal(as.vector(scale_breaks(s)), letters[1:4])
  expect_equal(as.vector(scale_labels(s)), c("a", "b", "C", "D"))

  s <- scale_x_discrete(limits = letters[1:3], labels = labels)
  expect_equal(as.vector(scale_breaks(s)), letters[1:3])
  expect_equal(as.vector(scale_labels(s)), LETTERS[1:3])

})

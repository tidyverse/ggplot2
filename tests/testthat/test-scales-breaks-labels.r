context("Scales: breaks and labels")

test_that("labels match breaks, even when outside limits", {
  sc <- scale_y_continuous(breaks = 1:4, labels = 1:4, limits = c(1, 3))

  expect_equal(sc$get_breaks(), c(1:3, NA))
  expect_equal(sc$get_labels(), 1:4)
  expect_equal(sc$get_breaks_minor(), c(1, 1.5, 2, 2.5, 3))
})

test_that("labels must match breaks", {
  expect_error(scale_x_discrete(breaks = 1:3, labels = 1:2),
    "must have the same length")
  expect_error(scale_x_continuous(breaks = 1:3, labels = 1:2),
    "must have the same length")
})

test_that("labels don't have to match null breaks", {
  expect_true(check_breaks_labels(breaks = 1:3, labels = NULL))
  expect_true(check_breaks_labels(breaks = NULL, labels = 1:2))
})


test_that("labels don't have extra spaces", {
  labels <- c("a", "abc", "abcdef")

  sc1 <- scale_x_discrete(limits = labels)
  sc2 <- scale_fill_discrete(limits = labels)

  expect_equal(sc1$get_labels(), labels)
  expect_equal(sc2$get_labels(), labels)

})


test_that("out-of-range breaks are dropped", {
  # Limits are explicitly specified, automatic labels
  sc <- scale_x_continuous(breaks = 1:5, limits = c(2, 4))
  bi <- sc$break_info()
  expect_equal(bi$labels, as.character(2:4))
  expect_equal(bi$major, c(0, 0.5, 1))
  expect_equal(bi$major_source, 2:4)


  # Limits and labels are explicitly specified
  sc <- scale_x_continuous(breaks = 1:5, labels = letters[1:5], limits = c(2, 4))
  bi <- sc$break_info()
  expect_equal(bi$labels, letters[2:4])
  expect_equal(bi$major, c(0, 0.5, 1))
  expect_equal(bi$major_source, 2:4)


  # Limits are specified, and all breaks are out of range
  sc <- scale_x_continuous(breaks = c(1,5), labels = letters[c(1,5)], limits = c(2, 4))
  bi <- sc$break_info()
  expect_equal(length(bi$labels), 0)
  expect_equal(length(bi$major), 0)
  expect_equal(length(bi$major_source), 0)


  # limits aren't specified, automatic labels
  # limits are set by the data
  sc <- scale_x_continuous(breaks = 1:5)
  sc$train_df(data.frame(x = 2:4))
  bi <- sc$break_info()
  expect_equal(bi$labels, as.character(2:4))
  expect_equal(bi$major_source, 2:4)
  expect_equal(bi$major, c(0, 0.5, 1))


  # Limits and labels are specified
  sc <- scale_x_continuous(breaks = 1:5, labels = letters[1:5])
  sc$train_df(data.frame(x = 2:4))
  bi <- sc$break_info()
  expect_equal(bi$labels, letters[2:4])
  expect_equal(bi$major_source, 2:4)
  expect_equal(bi$major, c(0, 0.5, 1))


  # Limits aren't specified, and all breaks are out of range of data
  sc <- scale_x_continuous(breaks = c(1,5), labels = letters[c(1,5)])
  sc$train_df(data.frame(x = 2:4))
  bi <- sc$break_info()
  expect_equal(length(bi$labels), 0)
  expect_equal(length(bi$major), 0)
  expect_equal(length(bi$major_source), 0)
})


test_that("no minor breaks when only one break", {
  sc1 <- scale_x_discrete(limits = "a")
  sc2 <- scale_x_continuous(limits = 1)

  expect_equal(length(sc1$get_breaks_minor()), 0)
  expect_equal(length(sc2$get_breaks_minor()), 0)

})

init_scale <- function(...) {
  sc <- scale_x_discrete(...)
  sc$train(factor(1:100))
  expect_equal(length(sc$get_limits()), 100)
  sc
}

test_that("discrete labels match breaks", {

  sc <- init_scale(breaks = 0:5 * 10)
  expect_equal(length(sc$get_breaks()), 5)
  expect_equal(length(sc$get_labels()), 5)
  expect_equivalent(sc$get_labels(), sc$get_breaks())

  sc <- init_scale(breaks = 0:5 * 10, labels = letters[1:6])
  expect_equal(length(sc$get_breaks()), 5)
  expect_equal(length(sc$get_labels()), 5)
  expect_equal(sc$get_labels(), letters[2:6])

  sc <- init_scale(breaks = 0:5 * 10, labels =
    function(x) paste(x, "-", sep = ""))
  expect_equal(sc$get_labels(), c("10-", "20-", "30-", "40-", "50-"))

  pick_5 <- function(x) sample(x, 5)
  sc <- init_scale(breaks = pick_5)
  expect_equal(length(sc$get_breaks()), 5)
  expect_equal(length(sc$get_labels()), 5)

})


test_that("scale breaks with numeric log transformation", {
  sc <- scale_x_continuous(limits = c(1, 1e5), trans = log10_trans())
  expect_equal(sc$get_breaks(), c(0, 2, 4)) # 1, 100, 10000
  expect_equal(sc$get_breaks_minor(), c(0, 1, 2, 3, 4, 5))
})

test_that("continuous scales with no data have no breaks or labels", {
  sc <- scale_x_continuous()

  expect_equal(sc$get_breaks(), numeric())
  expect_equal(sc$get_labels(), character())
  expect_equal(sc$get_limits(), c(0, 1))

})

test_that("discrete scales with no data have no breaks or labels", {
  sc <- scale_x_discrete()

  expect_equal(sc$get_breaks(), numeric())
  expect_equal(sc$get_labels(), character())
  expect_equal(sc$get_limits(), c(0, 1))
})

test_that("suppressing breaks, minor_breask, and labels", {
  expect_equal(scale_x_continuous(breaks = NULL, limits = c(1, 3))$get_breaks(), NULL)
  expect_equal(scale_x_discrete(breaks = NULL, limits = c(1, 3))$get_breaks(), NULL)
  expect_equal(scale_x_continuous(minor_breaks = NULL, limits = c(1, 3))$get_breaks_minor(), NULL)

  expect_equal(scale_x_continuous(labels = NULL, limits = c(1, 3))$get_labels(), NULL)
  expect_equal(scale_x_discrete(labels = NULL, limits = c(1, 3))$get_labels(), NULL)

  # date, datetime
  lims <- as.Date(c("2000/1/1", "2000/2/1"))
  expect_equal(scale_x_date(breaks = NULL, limits = lims)$get_breaks(), NULL)
  # NA is defunct, should throw error
  expect_error(scale_x_date(breaks = NA, limits = lims)$get_breaks())
  expect_equal(scale_x_date(labels = NULL, limits = lims)$get_labels(), NULL)
  expect_error(scale_x_date(labels = NA, limits = lims)$get_labels())
  expect_equal(scale_x_date(minor_breaks = NULL, limits = lims)$get_breaks_minor(), NULL)
  expect_error(scale_x_date(minor_breaks = NA, limits = lims)$get_breaks_minor())

  # date, datetime
  lims <- as.POSIXct(c("2000/1/1 0:0:0", "2010/1/1 0:0:0"))
  expect_equal(scale_x_datetime(breaks = NULL, limits = lims)$get_breaks(), NULL)
  expect_error(scale_x_datetime(breaks = NA, limits = lims)$get_breaks())
  expect_equal(scale_x_datetime(labels = NULL, limits = lims)$get_labels(), NULL)
  expect_error(scale_x_datetime(labels = NA, limits = lims)$get_labels())
  expect_equal(scale_x_datetime(minor_breaks = NULL, limits = lims)$get_breaks_minor(), NULL)
  expect_error(scale_x_datetime(minor_breaks = NA, limits = lims)$get_breaks_minor())

})

test_that("scale_breaks with explicit NA options (deprecated)", {
  # NA is defunct, should throw error

  # X
  sxc <- scale_x_continuous(breaks = NA)
  sxc$train(1:3)
  expect_error(sxc$get_breaks())
  expect_error(sxc$get_breaks_minor())

  # Y
  syc <- scale_y_continuous(breaks = NA)
  syc$train(1:3)
  expect_error(syc$get_breaks())
  expect_error(syc$get_breaks_minor())

  # Alpha
  sac <- scale_alpha_continuous(breaks = NA)
  sac$train(1:3)
  expect_error(sac$get_breaks())

  # Size
  ssc <- scale_size_continuous(breaks = NA)
  ssc$train(1:3)
  expect_error(ssc$get_breaks())

  # Fill
  sfc <- scale_fill_continuous(breaks = NA)
  sfc$train(1:3)
  expect_error(sfc$get_breaks())

  # Colour
  scc <- scale_colour_continuous(breaks = NA)
  scc$train(1:3)
  expect_error(scc$get_breaks())

})


test_that("breaks can be specified by names of labels", {
  labels <- setNames(LETTERS[1:4], letters[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = labels)
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = rev(labels))
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = labels[1:2])
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), c("A", "B", "c", "d"))

  s <- scale_x_discrete(limits = letters[1:4], labels = labels[3:4])
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), c("a", "b", "C", "D"))

  s <- scale_x_discrete(limits = letters[1:3], labels = labels)
  expect_equal(as.vector(s$get_breaks()), letters[1:3])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:3])

})

test_that("only finite or NA values for breaks for transformed scales (#871)", {
  sc <- scale_y_continuous(limits = c(0.01, 0.99), trans = "probit",
                           breaks = seq(0, 1, 0.2))
  breaks <- sc$get_breaks()
  expect_true(all(is.finite(breaks) | is.na(breaks)))
})

test_that("minor breaks are transformed by scales", {
  sc <- scale_y_continuous(limits = c(1, 100), trans = "log10",
    minor_breaks = c(1, 10, 100))

  expect_equal(sc$get_breaks_minor(), c(0, 1, 2))
})


# Visual tests ------------------------------------------------------------

test_that("minor breaks draws correctly", {
  p <- ggplot(NULL, aes(1:3, 1:3)) + geom_point() +
    scale_x_continuous(breaks = 1:3, minor_breaks = c(1.25, 2.75)) +
    scale_y_continuous(breaks = 1:3, minor_breaks = c(1.25, 2.75))

  vdiffr::expect_doppelganger("manual minor breaks", p)
  vdiffr::expect_doppelganger("manual minor breaks with coord_polar",
    p + coord_polar()
  )

  set.seed(342)
  df <- data.frame(
    date = seq(as.Date("2012-2-29"), length.out = 100, by = "1 day")[sample(100, 50)],
    price = runif(50)
  )
  df <- df[order(df$date), ]
  library(scales)
  p <- qplot(date, price, data = df, geom = "line") +
    scale_x_date(
      labels = date_format("%m/%d"),
      breaks = date_breaks("month"),
      minor_breaks = date_breaks("week")
    )

  vdiffr::expect_doppelganger("months and weeks breaks", p)
  vdiffr::expect_doppelganger("months and weeks breaks: coord polar", p + coord_polar())
  vdiffr::expect_doppelganger("default breaks",
    ggplot(NULL, aes(letters[1:3], 1:3)) + geom_point()
  )
  vdiffr::expect_doppelganger("scale_x_continuous(trans = log2_trans()) + scale_y_log10",
    qplot(1:1e4, 1:1e4) + scale_x_continuous(trans = log2_trans()) + scale_y_log10()
  )
  vdiffr::expect_doppelganger("x and y transformations",
    qplot(1:5, 1:5) + scale_x_continuous(trans = exp_trans(2)) + scale_y_continuous(trans = exp_trans(2)) +
      ggtitle("scale_x_continuous(trans = exp_trans(2)) + scale_y_continuous(trans = exp_trans(2))")
  )
})

test_that("scale breaks can be removed", {
  dat <- data.frame(x = 1:3, y = 1:3)

  vdiffr::expect_doppelganger("no x breaks",
    ggplot(dat, aes(x = x, y = y)) + geom_point() + scale_x_continuous(breaks = NULL)
  )
  vdiffr::expect_doppelganger("no y breaks",
    ggplot(dat, aes(x = x, y = y)) + geom_point() + scale_y_continuous(breaks = NULL)
  )
  vdiffr::expect_doppelganger("no alpha breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, alpha = x)) + geom_point() + scale_alpha_continuous(breaks = NULL)
  )
  vdiffr::expect_doppelganger("no size breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, size = x)) + geom_point() + scale_size_continuous(breaks = NULL)
  )
  vdiffr::expect_doppelganger("no fill breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, fill = x)) + geom_point(shape = 21) + scale_fill_continuous(breaks = NULL)
  )
  vdiffr::expect_doppelganger("no colour breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, colour = x)) + geom_point() + scale_colour_continuous(breaks = NULL)
  )
})

skip_on_cran() # This test suite is long-running (on cran) and is skipped

set.seed(111)
dat <- data_frame(x = rep(LETTERS[1:2], 15), y = rnorm(30), g = rep(LETTERS[3:5], 10))

test_that("dodging works", {
  p <- ggplot(dat, aes(x = x, y = y, fill = g)) +
    geom_dotplot(
      binwidth = 0.2,
      binaxis = "y",
      position = "dodge",
      stackdir = "center"
    )
  df <- get_layer_data(p)

  # Number of levels in the dodged variable
  ndodge <- 3

  # The amount of space allocated within each dodge group
  dwidth <- 0.9 / ndodge

  # This should be the x position for each before dodging
  xbase <- ceiling(df$group / ndodge)

  # This is the offset from dodging
  xoffset <- (df$group - 1) %% ndodge - (ndodge - 1) / 2
  xoffset <- xoffset * dwidth

  # Check actual x locations equal predicted x locations
  expect_true(all(abs(df$x - (xbase + xoffset)) < 1e-6))

  # Check that xmin and xmax are in the right place
  expect_true(all(abs(df$xmax - df$x - dwidth/2) < 1e-6))
  expect_true(all(abs(df$x - df$xmin - dwidth/2) < 1e-6))
})

test_that("binning works", {
  bp <- ggplot(dat, aes(y)) +
    geom_dotplot(binwidth = 0.4, method = "histodot")
  x <- get_layer_data(bp)$x

  # Need ugly hack to make sure mod function doesn't give values like -3.99999
  # due to floating point error
  expect_true(all(abs((x - min(x) + 1e-7) %% 0.4) < 1e-6))

  bp <- ggplot(dat, aes(x = y)) +
    geom_dotplot(binwidth = 0.4, method = "dotdensity")
  x <- get_layer_data(bp)$x

  # This one doesn't ensure that dotdensity works, but it does check that it's not
  # doing fixed bin sizes
  expect_false(all(abs((x - min(x) + 1e-7) %% 0.4) < 1e-6))
})

test_that("NA's result in warning from stat_bindot", {
  set.seed(122)
  dat <- data_frame(x = rnorm(20))
  dat$x[c(2,10)] <- NA

  # Need to assign it to a var here so that it doesn't automatically print
  expect_snapshot_warning(ggplot_build(ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.2)))
})

test_that("when binning on y-axis, limits depend on the panel", {
   p <- ggplot(mtcars, aes(factor(cyl), mpg)) +
        geom_dotplot(binaxis='y', binwidth = 1/30 * diff(range(mtcars$mpg)))

   b1 <- ggplot_build(p + facet_wrap(~am))
   b2 <- ggplot_build(p + facet_wrap(~am, scales = "free_y"))

   equal_limits1 <- (b1@layout$panel_params[[1]]$y.range == b1@layout$panel_params[[2]]$y.range)
   equal_limits2 <- (b2@layout$panel_params[[1]]$y.range == b2@layout$panel_params[[2]]$y.range)

   expect_true(all(equal_limits1))
   expect_false(all(equal_limits2))
})

test_that("weight aesthetic is checked", {
  p <- ggplot(mtcars, aes(x = mpg, weight = gear/3)) +
    geom_dotplot(binwidth = 1/30 * diff(range(mtcars$mpg)))
  expect_snapshot_warning(ggplot_build(p))
  p <- ggplot(mtcars, aes(x = mpg, weight = -gear)) +
    geom_dotplot(binwidth = 1/30 * diff(range(mtcars$mpg)))
  expect_snapshot_warning(ggplot_build(p))
})

# Visual tests ------------------------------------------------------------

test_that("geom_dotplot draws correctly", {
  set.seed(112)
  dat <- data_frame(x = rnorm(20), g = rep(LETTERS[1:2], 10))

  # Basic dotplot with binning along x axis
  expect_doppelganger("basic dotplot with dot-density binning, binwidth = .4",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4)
  )
  expect_doppelganger("histodot binning (equal bin spacing)",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, method = "histodot")
  )
  expect_doppelganger("dots stacked closer: stackratio=.5, fill=white",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackratio = 0.5, fill = "white")
  )
  expect_doppelganger("larger dots: dotsize=1.5, fill=white",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, dotsize = 1.4, fill = "white")
  )

  # Stacking methods
  expect_doppelganger("stack up",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackdir = "up")
  )
  expect_doppelganger("stack down",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackdir = "down")
  )
  expect_doppelganger("stack center",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackdir = "center")
  )
  expect_doppelganger("stack centerwhole",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackdir = "centerwhole")
  )

  # Stacking methods with coord_flip
  expect_doppelganger("stack up with coord_flip",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackdir = "up") + coord_flip()
  )
  expect_doppelganger("stack down with coord_flip",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackdir = "down") + coord_flip()
  )
  expect_doppelganger("stack center with coord_flip",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackdir = "center") + coord_flip()
  )
  expect_doppelganger("stack centerwhole with coord_flip",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.4, stackdir = "centerwhole") + coord_flip()
  )

  # Binning along x, with groups
  expect_doppelganger("multiple groups, bins not aligned",
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = 0.4, alpha = 0.4)
  )
  expect_doppelganger("multiple groups, bins aligned",
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = 0.4, alpha = 0.4, binpositions = "all")
  )

  # Binning along y axis
  expect_doppelganger("bin along y, stack center",
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = 0.4, binaxis = "y", stackdir = "center")
  )
  expect_doppelganger("bin along y, stack centerwhole",
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = 0.4, binaxis = "y", stackdir = "centerwhole")
  )
  expect_doppelganger("bin along y, stack centerwhole, histodot",
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = 0.4, binaxis = "y", stackdir = "centerwhole", method = "histodot")
  )

  # Binning along y, with multiple grouping factors
  dat2 <- data_frame(x = rep(factor(LETTERS[1:3]), 30), y = rnorm(90), g = rep(factor(LETTERS[1:2]), 45))

  expect_doppelganger("bin x, three y groups, stack centerwhole",
    ggplot(dat2, aes(y, x)) + geom_dotplot(binwidth = 0.25, binaxis = "x", stackdir = "centerwhole")
  )
  expect_doppelganger("bin y, three x groups, stack centerwhole",
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = 0.25, binaxis = "y", stackdir = "centerwhole")
  )
  expect_doppelganger("bin y, three x groups, bins aligned across groups",
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = 0.25, binaxis = "y", stackdir = "center", binpositions = "all")
  )
  expect_doppelganger("bin y, three x groups, bins aligned, coord_flip",
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = 0.25, binaxis = "y", stackdir = "center", binpositions = "all") +
      coord_flip()
  )
  expect_doppelganger("bin y, dodged",
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, 0.4)) +
      geom_dotplot(binwidth = 0.25, position = "dodge", binaxis = "y", stackdir = "center")
  )
  expect_doppelganger("bin y, dodged, coord_flip",
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, 0.4)) +
      geom_dotplot(binwidth = 0.25, position = "dodge", binaxis = "y", stackdir = "center") +
      coord_flip()
  )
  expect_doppelganger("bin y, three x groups, fill and dodge",
    ggplot(dat2, aes(x, y, fill = g)) + scale_y_continuous(breaks = seq(-4 ,4, 0.4)) +
      geom_dotplot(binwidth = 0.2, position = "dodge", binaxis = "y", stackdir = "center")
  )
  expect_doppelganger("bin y, continous x-axis, grouping by x",
    ggplot(dat2, aes(as.numeric(x), y, group = x)) + geom_dotplot(binwidth = 0.2, binaxis = "y", stackdir = "center")
  )
  expect_doppelganger("bin y, continous x-axis, single x group",
    ggplot(dat2, aes(as.numeric(x), y)) + geom_dotplot(binwidth = 0.2, binaxis = "y", stackdir = "center")
  )

  # border width and size
  expect_doppelganger(
    "variable linetype and size specified as aesthetics",
    ggplot(
      dat,
      aes(
        x,
        linetype = rep(c("a", "b"), length.out = nrow(dat)),
        stroke = rep(c(1, 2), length.out = nrow(dat))
      )
    ) +
      geom_dotplot(binwidth = 0.4, fill = "red", col = "blue") +
      continuous_scale("stroke", palette = function(x) scales::rescale(x, to = c(1, 6))) +
      guides(linetype = guide_legend(order = 1))
  )

  # Stacking groups
  expect_doppelganger("3 stackgroups, dot-density with aligned bins",
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = 0.25, stackgroups = TRUE, binpositions = "all", alpha = 0.5)
  )
  expect_doppelganger("3 stackgroups, histodot",
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = 0.25, stackgroups = TRUE, method = "histodot", alpha = 0.5)
  )
  expect_doppelganger("3 stackgroups, bin y, histodot",
    ggplot(dat2, aes(1, y, fill = x)) + geom_dotplot(binaxis = "y", binwidth = 0.25, stackgroups = TRUE, method = "histodot", alpha = 0.5)
  )

  # This one is currently broken but it would be a really rare case, and it
  # probably requires a really ugly hack to fix
  expect_doppelganger("bin y, dodging, 3 stackgroups, histodot",
    ggplot(dat2, aes(x, y, fill = g)) +
      geom_dotplot(binaxis = "y", binwidth = 0.25, stackgroups = TRUE, method = "histodot",
                   alpha = 0.5, stackdir = "centerwhole")
  )
  expect_doppelganger("facets, 3 groups, histodot, stackgroups",
    ggplot(dat2, aes(y, fill = g)) + geom_dotplot(binwidth = 0.25, stackgroups = TRUE, method = "histodot", alpha = 0.5) +
      facet_grid(x ~ .)
  )

  # Missing values
  dat2 <- dat
  dat2$x[c(1, 10)] <- NA

  expect_snapshot_warning(expect_doppelganger(
    "2 NA values, dot-density binning, binwidth = .4",
    ggplot(dat2, aes(x)) + geom_dotplot(binwidth = 0.4)
  ))
  expect_snapshot_warning(expect_doppelganger(
    "2 NA values, bin along y, stack center",
    ggplot(dat2, aes(0, x)) + geom_dotplot(binwidth = 0.4, binaxis = "y", stackdir = "center")
  ))
})

test_that("stackratio != 1 works", {
  df <- data.frame(x = c(rep(1, 3), rep(2, 2)))

  expect_doppelganger("stackratio = 1.5",
    ggplot(df) +
      geom_hline(yintercept = 0) +
      geom_dotplot(aes(x), binwidth = 0.5, stackdir = "down", stackratio = 1.5, fill = NA) +
      geom_dotplot(aes(x + 3), binwidth = 0.5, stackdir = "up", stackratio = 1.5, fill = NA) +
      geom_dotplot(aes(x + 6), binwidth = 0.5, stackdir = "center", stackratio = 1.5, fill = NA) +
      geom_dotplot(aes(x + 9), binwidth = 0.5, stackdir = "centerwhole", stackratio = 1.5, fill = NA)
  )
})

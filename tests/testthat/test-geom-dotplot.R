context("geom-dotplot")


# Visual tests ------------------------------------------------------------

test_that("geom_dotplot draws correctly", {
  set.seed(112)
  dat <- data.frame(x = rnorm(20), g = LETTERS[1:2])

  # Basic dotplot with binning along x axis
  vdiffr::expect_doppelganger("basic dotplot with dot-density binning, binwidth = .4",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4)
  )
  vdiffr::expect_doppelganger("histodot binning (equal bin spacing)",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, method = "histodot")
  )
  vdiffr::expect_doppelganger("dots stacked closer: stackratio=.5, fill=white",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackratio = .5, fill = "white")
  )
  vdiffr::expect_doppelganger("larger dots: dotsize=1.5, fill=white",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, dotsize = 1.4, fill = "white")
  )

  # Stacking methods
  vdiffr::expect_doppelganger("stack up",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "up")
  )
  vdiffr::expect_doppelganger("stack down",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "down")
  )
  vdiffr::expect_doppelganger("stack center",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "center")
  )
  vdiffr::expect_doppelganger("stack centerwhole",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "centerwhole")
  )

  # Stacking methods with coord_flip
  vdiffr::expect_doppelganger("stack up with coord_flip",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "up") + coord_flip()
  )
  vdiffr::expect_doppelganger("stack down with coord_flip",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "down") + coord_flip()
  )
  vdiffr::expect_doppelganger("stack center with coord_flip",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "center") + coord_flip()
  )
  vdiffr::expect_doppelganger("stack centerwhole with coord_flip",
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "centerwhole") + coord_flip()
  )

  # Binning along x, with groups
  vdiffr::expect_doppelganger("multiple groups, bins not aligned",
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = .4, alpha = .4)
  )
  vdiffr::expect_doppelganger("multiple groups, bins aligned",
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = .4, alpha = .4, binpositions = "all")
  )

  # Binning along y axis
  vdiffr::expect_doppelganger("bin along y, stack center",
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "center")
  )
  vdiffr::expect_doppelganger("bin along y, stack centerwhole",
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "centerwhole")
  )
  vdiffr::expect_doppelganger("bin along y, stack centerwhole, histodot",
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "centerwhole", method = "histodot")
  )

  # Binning along y, with multiple grouping factors
  dat2 <- data.frame(x = LETTERS[1:3], y = rnorm(90), g = LETTERS[1:2])

  vdiffr::expect_doppelganger("bin y, three x groups, stack centerwhole",
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "centerwhole")
  )
  vdiffr::expect_doppelganger("bin y, three x groups, bins aligned across groups",
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "center", binpositions = "all")
  )
  vdiffr::expect_doppelganger("bin y, three x groups, bins aligned, coord_flip",
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "center", binpositions = "all") +
      coord_flip()
  )
  vdiffr::expect_doppelganger("bin y, dodged",
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, .4)) +
      geom_dotplot(binwidth = .25, position = "dodge", binaxis = "y", stackdir = "center")
  )
  vdiffr::expect_doppelganger("bin y, dodged, coord_flip",
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, .4)) +
      geom_dotplot(binwidth = .25, position = "dodge", binaxis = "y", stackdir = "center") +
      coord_flip()
  )
  vdiffr::expect_doppelganger("bin y, three x groups, fill and dodge",
    ggplot(dat2, aes(x, y, fill = g)) + scale_y_continuous(breaks = seq(-4 ,4, .4)) +
      geom_dotplot(binwidth = .2, position = "dodge", binaxis = "y", stackdir = "center")
  )
  vdiffr::expect_doppelganger("bin y, continous x-axis, grouping by x",
    ggplot(dat2, aes(as.numeric(x), y, group = x)) + geom_dotplot(binwidth = .2, binaxis = "y", stackdir = "center")
  )
  vdiffr::expect_doppelganger("bin y, continous x-axis, single x group",
    ggplot(dat2, aes(as.numeric(x), y)) + geom_dotplot(binwidth = .2, binaxis = "y", stackdir = "center")
  )

  # Stacking groups
  vdiffr::expect_doppelganger("stackgroups with 3 groups, dot-density with aligned bins",
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, binpositions = "all", alpha = 0.5)
  )
  vdiffr::expect_doppelganger("stackgroups with 3 groups, histodot",
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5)
  )
  vdiffr::expect_doppelganger("stackgroups with 3 groups, bin y, histodot",
    ggplot(dat2, aes(1, y, fill = x)) + geom_dotplot(binaxis = "y", binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5)
  )

  # This one is currently broken but it would be a really rare case, and it
  # probably requires a really ugly hack to fix
  vdiffr::expect_doppelganger("bin y, dodging, stackgroups with 3 groups, histodot (currently broken)",
    ggplot(dat2, aes(x, y, fill = g)) +
      geom_dotplot(binaxis = "y", binwidth = .25, stackgroups = TRUE, method = "histodot",
                   alpha = 0.5, stackdir = "centerwhole")
  )
  vdiffr::expect_doppelganger("facets, 3 groups, histodot, stackgroups",
    ggplot(dat2, aes(y, fill = g)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5) +
      facet_grid(x ~ .)
  )

  # Missing values
  dat2 <- dat
  dat2$x[c(1, 10)] <- NA

  vdiffr::expect_doppelganger("2 NA values, dot-density binning, binwidth = .4",
    ggplot(dat2, aes(x)) + geom_dotplot(binwidth = .4)
  )
  vdiffr::expect_doppelganger("2 NA values, bin along y, stack center",
    ggplot(dat2, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "center")
  )
})

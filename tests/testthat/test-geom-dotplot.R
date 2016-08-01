context("geom-dotplot")


# Visual tests ------------------------------------------------------------

test_that("geom_dotplot draws correctly", {
  set.seed(112)
  dat <- data.frame(x = rnorm(20), g = LETTERS[1:2])

  # Basic dotplot with binning along x axis
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4),
    "basic dotplot with dot-density binning, binwidth = .4"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, method = "histodot"),
    "histodot binning (equal bin spacing)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackratio = .5, fill = "white"),
    "dots stacked closer: stackratio=.5, fill=white"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, dotsize = 1.4, fill = "white"),
    "larger dots: dotsize=1.5, fill=white"
  )

  # Stacking methods
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "up"),
    "stack up"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "down"),
    "stack down"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "center"),
    "stack center"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "centerwhole"),
    "stack centerwhole"
  )

  # Stacking methods with coord_flip
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "up") + coord_flip(),
    "stack up with coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "down") + coord_flip(),
    "stack down with coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "center") + coord_flip(),
    "stack center with coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "centerwhole") + coord_flip(),
    "stack centerwhole with coord_flip"
  )

  # Binning along x, with groups
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = .4, alpha = .4),
    "multiple groups, bins not aligned"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = .4, alpha = .4, binpositions = "all"),
    "multiple groups, bins aligned"
  )

  # Binning along y axis
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "center"),
    "bin along y, stack center"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "centerwhole"),
    "bin along y, stack centerwhole"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "centerwhole", method = "histodot"),
    "bin along y, stack centerwhole, histodot"
  )

  # Binning along y, with multiple grouping factors
  dat2 <- data.frame(x = LETTERS[1:3], y = rnorm(90), g = LETTERS[1:2])

  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "centerwhole"),
    "bin y, three x groups, stack centerwhole"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "center", binpositions = "all"),
    "bin y, three x groups, bins aligned across groups"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "center", binpositions = "all") +
      coord_flip(),
    "bin y, three x groups, bins aligned, coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, .4)) +
      geom_dotplot(binwidth = .25, position = "dodge", binaxis = "y", stackdir = "center"),
    "bin y, dodged"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, .4)) +
      geom_dotplot(binwidth = .25, position = "dodge", binaxis = "y", stackdir = "center") +
      coord_flip(),
    "bin y, dodged, coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y, fill = g)) + scale_y_continuous(breaks = seq(-4 ,4, .4)) +
      geom_dotplot(binwidth = .2, position = "dodge", binaxis = "y", stackdir = "center"),
    "bin y, three x groups, fill and dodge"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(as.numeric(x), y, group = x)) + geom_dotplot(binwidth = .2, binaxis = "y", stackdir = "center"),
    "bin y, continous x-axis, grouping by x"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(as.numeric(x), y)) + geom_dotplot(binwidth = .2, binaxis = "y", stackdir = "center"),
    "bin y, continous x-axis, single x group"
  )

  # Stacking groups
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, binpositions = "all", alpha = 0.5),
    "stackgroups with 3 groups, dot-density with aligned bins"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5),
    "stackgroups with 3 groups, histodot"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(1, y, fill = x)) + geom_dotplot(binaxis = "y", binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5),
    "stackgroups with 3 groups, bin y, histodot"
  )

  # This one is currently broken but it would be a really rare case, and it
  # probably requires a really ugly hack to fix
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y, fill = g)) +
      geom_dotplot(binaxis = "y", binwidth = .25, stackgroups = TRUE, method = "histodot",
                   alpha = 0.5, stackdir = "centerwhole"),
    "bin y, dodging, stackgroups with 3 groups, histodot (currently broken)"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = g)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5) +
      facet_grid(x ~ .),
    "facets, 3 groups, histodot, stackgroups"
  )

  # Missing values
  dat2 <- dat
  dat2$x[c(1, 10)] <- NA

  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x)) + geom_dotplot(binwidth = .4),
    "2 NA values, dot-density binning, binwidth = .4"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "center"),
    "2 NA values, bin along y, stack center"
  )
})

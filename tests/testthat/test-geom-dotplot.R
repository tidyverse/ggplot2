context("geom-dotplot")


# Visual tests ------------------------------------------------------------

test_that("geom_dotplot draws correctly", {
  set.seed(112)
  dat <- data.frame(x = rnorm(20), g = LETTERS[1:2])

  # Basic dotplot with binning along x axis
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4),
    "basic_dotplot_with_dot-density_binning_binwidth-04"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, method = "histodot"),
    "histodot_binning_equal_bin_spacing"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackratio = .5, fill = "white"),
    "dots_stacked_closer_stackratio-05_fill-white"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, dotsize = 1.4, fill = "white"),
    "larger_dots_dotsize-14_fill-white"
  )

  # Stacking methods
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "up"),
    "stack_up"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "down"),
    "stack_down"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "center"),
    "stack_center"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "centerwhole"),
    "stack_centerwhole"
  )

  # Stacking methods with coord_flip
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "up") + coord_flip(),
    "stack_up_with_coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "down") + coord_flip(),
    "stack_down_with_coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "center") + coord_flip(),
    "stack_center_with_coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x)) + geom_dotplot(binwidth = .4, stackdir = "centerwhole") + coord_flip(),
    "stack_centerwhole_with_coord_flip"
  )

  # Binning along x, with groups
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = .4, alpha = .4),
    "multiple_groups_bins_not_aligned"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(x, fill = g)) + geom_dotplot(binwidth = .4, alpha = .4, binpositions = "all"),
    "multiple_groups_bins_aligned"
  )

  # Binning along y axis
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "center"),
    "bin_along_y_stack_center"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "centerwhole"),
    "bin_along_y_stack_centerwhole"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "centerwhole", method = "histodot"),
    "bin_along_y_stack_centerwhole_histodot"
  )

  # Binning along y, with multiple grouping factors
  dat2 <- data.frame(x = LETTERS[1:3], y = rnorm(90), g = LETTERS[1:2])

  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "centerwhole"),
    "bin_y_three_x_groups_stack_centerwhole"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "center", binpositions = "all"),
    "bin_y_three_x_groups_bins_aligned_across_groups"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y)) + geom_dotplot(binwidth = .25, binaxis = "y", stackdir = "center", binpositions = "all") +
      coord_flip(),
    "bin_y_three_x_groups_bins_aligned_coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, .4)) +
      geom_dotplot(binwidth = .25, position = "dodge", binaxis = "y", stackdir = "center"),
    "bin_y_dodged"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes("foo", y, fill = x)) + scale_y_continuous(breaks = seq(-4, 4, .4)) +
      geom_dotplot(binwidth = .25, position = "dodge", binaxis = "y", stackdir = "center") +
      coord_flip(),
    "bin_y_dodged_coord_flip"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y, fill = g)) + scale_y_continuous(breaks = seq(-4 ,4, .4)) +
      geom_dotplot(binwidth = .2, position = "dodge", binaxis = "y", stackdir = "center"),
    "bin_y_three_x_groups_fill_and_dodge"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(as.numeric(x), y, group = x)) + geom_dotplot(binwidth = .2, binaxis = "y", stackdir = "center"),
    "bin_y_continous_x-axis_grouping_by_x"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(as.numeric(x), y)) + geom_dotplot(binwidth = .2, binaxis = "y", stackdir = "center"),
    "bin_y_continous_x-axis_single_x_group"
  )

  # Stacking groups
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, binpositions = "all", alpha = 0.5),
    "stackgroups_with_3_groups_dot-density_with_aligned_bins"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = x)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5),
    "stackgroups_with_3_groups_histodot"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(1, y, fill = x)) + geom_dotplot(binaxis = "y", binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5),
    "stackgroups_with_3_groups_bin_y_histodot"
  )

  # This one is currently broken but it would be a really rare case, and it
  # probably requires a really ugly hack to fix
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x, y, fill = g)) +
      geom_dotplot(binaxis = "y", binwidth = .25, stackgroups = TRUE, method = "histodot",
                   alpha = 0.5, stackdir = "centerwhole"),
    "bin_y_dodging_stackgroups_with_3_groups_histodot_currently_broken"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(y, fill = g)) + geom_dotplot(binwidth = .25, stackgroups = TRUE, method = "histodot", alpha = 0.5) +
      facet_grid(x ~ .),
    "facets_3_groups_histodot_stackgroups"
  )

  # Missing values
  dat2 <- dat
  dat2$x[c(1, 10)] <- NA

  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(x)) + geom_dotplot(binwidth = .4),
    "2_NA_values_dot-density_binning_binwidth-04"
  )
  vdiffr::expect_doppelganger(
    ggplot(dat2, aes(0, x)) + geom_dotplot(binwidth = .4, binaxis = "y", stackdir = "center"),
    "2_NA_values_bin_along_y_stack_center"
  )
})

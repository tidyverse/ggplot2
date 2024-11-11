skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("guide_none() can be used in non-position scales", {
  p <- ggplot(mpg, aes(cty, hwy, colour = class)) +
    geom_point() +
    scale_color_discrete(guide = guide_none())

  built <- ggplot_build(p)
  plot <- built$plot
  guides <- guides_list(plot$guides)
  guides <- guides$build(
    plot$scales,
    plot$layers,
    plot$labels
  )

  expect_length(guides$guides, 0)
})

test_that("guide specifications are properly checked", {
  expect_snapshot_error(validate_guide("test"))
  expect_snapshot_error(validate_guide(1))

  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, shape = factor(gear))) +
    guides(shape = "colourbar")

  expect_snapshot_warning(ggplotGrob(p))

  p <-  p + guides(shape = guide_legend(theme = theme(legend.title.position = "leftish")))
  expect_snapshot_error(ggplotGrob(p))

  expect_snapshot_error(guide_colourbar()$transform())

  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = gear)) +
    guides(colour = guide_colourbar(theme = theme(legend.text.position = "top")))
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = gear)) +
    guides(colour = guide_colourbar(direction = "horizontal", theme = theme(legend.text.position = "left")))
  expect_snapshot_error(ggplotGrob(p))

  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = gear)) +
    guides(colour = guide_legend(theme = theme(legend.text.position = "test")))
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = gear)) +
    guides(colour = guide_legend(nrow = 2, ncol = 2))
  expect_snapshot_error(ggplotGrob(p))
})

test_that("guide_coloursteps and guide_bins return ordered breaks", {
  scale <- scale_colour_viridis_c(breaks = c(2, 3, 1))
  scale$train(c(0, 4))

  # Coloursteps guide is increasing order
  g <- guide_colorsteps()
  key <- g$train(scale = scale, aesthetic = "colour")$key
  expect_true(all(diff(key$.value) > 0))

  # Bins guide is increasing order
  g <- guide_bins()
  key <- g$train(scale = scale, aesthetics = "colour")$key
  expect_true(all(diff(key$.value) > 0))

  # Out of bound breaks are removed
  scale <- scale_colour_viridis_c(breaks = c(10, 20, 30, 40, 50), na.value = "grey50")
  scale$train(c(15, 45))

  g <- guide_colorsteps()
  key <- g$train(scale = scale, aesthetic = "colour")$key
  expect_equal(sum(key$colour == "grey50"), 0)
})

test_that("guide_coloursteps can parse (un)even steps from discrete scales", {

  val <- cut(1:10, breaks = c(0, 3, 5, 10), include.lowest = TRUE)
  scale <- scale_colour_viridis_d()
  scale$train(val)

  g <- guide_coloursteps(even.steps = TRUE)
  decor <- g$train(scale = scale, aesthetics = "colour")$decor
  expect_equal(decor$max - decor$min, rep(1/3, 3))

  g <- guide_coloursteps(even.steps = FALSE)
  decor <- g$train(scale = scale, aesthetics = "colour")$decor
  expect_equal(decor$max - decor$min, c(0.3, 0.2, 0.5))
})

test_that("get_guide_data retrieves keys appropriately", {

  p <- ggplot(mtcars, aes(mpg, disp, colour = drat, size = drat, fill = wt)) +
    geom_point(shape = 21) +
    facet_wrap(vars(cyl), scales = "free_x") +
    guides(colour = "legend")
  b <- ggplot_build(p)

  # Test facetted panel
  test <- get_guide_data(b, "x", panel = 2)
  expect_equal(test$.label, c("18", "19", "20", "21"))

  # Test plain legend
  test <- get_guide_data(b, "fill")
  expect_equal(test$.label, c("2", "3", "4", "5"))

  # Test merged legend
  test <- get_guide_data(b, "colour")
  expect_true(all(c("colour", "size") %in% colnames(test)))

  # Unmapped data
  expect_null(get_guide_data(b, "shape"))

  # Non-existent panels
  expect_null(get_guide_data(b, "x", panel = 4))

  expect_snapshot(get_guide_data(b, 1), error = TRUE)
  expect_snapshot(get_guide_data(b, "x", panel = "a"), error = TRUE)
})

test_that("get_guide_data retrieves keys from exotic coords", {

  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()

  # Sanity check
  test <- get_guide_data(p + coord_cartesian(), "x")
  expect_equal(test$.label, c("10", "15", "20", "25", "30", "35"))

  # We're not testing the formatting, so just testing output shape
  test <- get_guide_data(p + coord_sf(crs = 3347), "y")
  expect_equal(nrow(test), 5)
  expect_true(all(c("x", ".value", ".label", "x") %in% colnames(test)))

  # For coords that don't use guide system, we expect a list
  test <- get_guide_data(p + coord_polar(), "theta")
  expect_true(is.list(test) && !is.data.frame(test))
  expect_equal(test$theta.labels, c("15", "20", "25", "30"))
})

test_that("empty guides are dropped", {

  df <- data.frame(x = 1:2)
  # Making a guide where all breaks are out-of-bounds
  p <- ggplot(df, aes(x, x, colour = x)) +
    geom_point() +
    scale_colour_continuous(
      limits = c(0.25, 0.75),
      breaks = c(1, 2),
      guide  = "legend"
    )
  p <- ggplot_build(p)

  # Empty guide that survives most steps
  gd <- get_guide_data(p, "colour")
  expect_equal(nrow(gd), 0)

  # Draw guides
  guides <- p$plot$guides$draw(theme_gray(), direction = "vertical")

  # All guide-boxes should be empty
  expect_equal(lengths(guides, use.names = FALSE), rep(0, 5))
})

test_that("bins can be parsed by guides for all scale types", {

  breaks <- c(90, 100, 200, 300)
  limits <- c(0, 1000)

  sc <- scale_colour_continuous(breaks = breaks)
  sc$train(limits)

  expect_equal(parse_binned_breaks(sc)$breaks, breaks)

  sc <- scale_colour_binned(breaks = breaks)
  sc$train(limits)

  expect_equal(parse_binned_breaks(sc)$breaks, breaks)

  # Note: discrete binned breaks treats outer breaks as limits
  cut <- cut(c(0, 95, 150, 250, 1000), breaks = breaks)

  sc <- scale_colour_discrete()
  sc$train(cut)

  parsed <- parse_binned_breaks(sc)
  expect_equal(
    sort(c(parsed$limits, parsed$breaks)),
    breaks
  )
})

# Visual tests ------------------------------------------------------------

test_that("guides are positioned correctly", {
  df <- data_frame(x = 1, y = 1, z = factor("a"))

  p1 <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    labs(title = "title of plot") +
    theme_test() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.background = element_rect(fill = "grey90"),
      legend.key = element_rect(fill = "grey90")
    ) +
    scale_x_continuous(breaks = 1, labels = "very long axis label") +
    scale_y_continuous(breaks = 1, labels = "very long axis label")

  expect_doppelganger("legend on left",
    p1 + theme(legend.position = "left")
  )
  expect_doppelganger("legend on bottom",
    p1 + theme(legend.position = "bottom")
  )
  expect_doppelganger("legend on right",
    p1 + theme(legend.position = "right")
  )
  expect_doppelganger("legend on top",
    p1 + theme(legend.position = "top")
  )
  expect_doppelganger("facet_grid, legend on left",
    p1 + facet_grid(x~y) + theme(legend.position = "left")
  )
  expect_doppelganger("facet_grid, legend on bottom",
    p1 + facet_grid(x~y) + theme(legend.position = "bottom")
  )
  expect_doppelganger("facet_grid, legend on right",
    p1 + facet_grid(x~y) + theme(legend.position = "right")
  )
  expect_doppelganger("facet_grid, legend on top",
    p1 + facet_grid(x~y) + theme(legend.position = "top")
  )
  expect_doppelganger("facet_wrap, legend on left",
    p1 + facet_wrap(~ x) + theme(legend.position = "left")
  )
  expect_doppelganger("facet_wrap, legend on bottom",
    p1 + facet_wrap(~ x) + theme(legend.position = "bottom")
  )
  expect_doppelganger("facet_wrap, legend on right",
    p1 + facet_wrap(~ x) + theme(legend.position = "right")
  )
  expect_doppelganger("facet_wrap, legend on top",
    p1 + facet_wrap(~ x) + theme(legend.position = "top")
  )

  # padding
  dat <- data_frame(x = LETTERS[1:3], y = 1)
  p2 <- ggplot(dat, aes(x, y, fill = x, colour = 1:3)) +
    geom_bar(stat = "identity") +
    guides(color = guide_colourbar(order = 1)) +
    theme_test() +
    theme(legend.background = element_rect(colour = "black"))

  expect_doppelganger("padding in legend box", p2)

  p2 <- p2 + theme(legend.position = "inside")
  # Placement of legend inside
  expect_doppelganger("legend inside plot, centered",
    p2 + theme(legend.position.inside = c(0.5, 0.5))
  )
  expect_doppelganger("legend inside plot, bottom left",
    p2 + theme(legend.justification = c(0,0), legend.position.inside = c(0,0))
  )
  expect_doppelganger("legend inside plot, top right",
    p2 + theme(legend.justification = c(1,1), legend.position.inside = c(1,1))
  )
  expect_doppelganger("legend inside plot, bottom left of legend at center",
    p2 + theme(legend.justification = c(0,0), legend.position.inside = c(0.5,0.5))
  )
})

test_that("guides title and text are positioned correctly", {
  df <- data_frame(x = 1:3, y = 1:3)
  p <- ggplot(df, aes(x, y, color = factor(x), fill = y)) +
    geom_point(shape = 21) +
    # setting the order explicitly removes the risk for failed doppelgangers
    # due to legends switching order
    guides(color = guide_legend(order = 2),
           fill = guide_colorbar(order = 1)) +
    theme_test()

  expect_doppelganger("multi-line guide title works",
    p +
      scale_color_discrete(name = "the\ndiscrete\ncolorscale") +
      scale_fill_continuous(name = "the\ncontinuous\ncolorscale")
  )
  expect_doppelganger("vertical gap of 1cm between guide title and guide",
    p + theme(legend.title = element_text(margin = margin(b = 1, unit = "cm")))
  )
  expect_doppelganger("horizontal gap of 1cm between guide and guide text",
    p + theme(legend.text = element_text(margin = margin(l = 1, unit = "cm")))
  )

  # now test label positioning, alignment, etc
  df <- data_frame(x = c(1, 10, 100))
  p <- ggplot(df, aes(x, x, color = x, size = x)) +
    geom_point() +
    # setting the order explicitly removes the risk for failed doppelgangers
    # due to legends switching order
    guides(shape = guide_legend(order = 1),
           color = guide_colorbar(order = 2)) +
    theme_test()

  expect_doppelganger("guide title and text positioning and alignment via themes",
    p + theme(
      legend.title = element_text(hjust = 0.5, margin = margin(t = 30, b = 5.5)),
      legend.text = element_text(hjust = 1, margin = margin(l = 10.5, t = 10, b = 10))
    )
  )

  # title and label rotation
  df <- data_frame(x = c(5, 10, 15))
  p <- ggplot(df, aes(x, x, color = x, fill = 15 - x)) +
    geom_point(shape = 21, size = 5, stroke = 3) +
    scale_colour_continuous(
      name = "value",
      guide = guide_colorbar(
        theme = theme(
          legend.title = element_text(size = 11, angle = 0, hjust = 0.5, vjust = 1),
          legend.text = element_text(size = 0.8 * 11, angle = 270, hjust = 0.5, vjust = 1)
        ),
        order = 2 # set guide order to keep visual test stable
      )
    ) +
    scale_fill_continuous(
      breaks = c(5, 10, 15),
      limits = c(5, 15),
      labels = paste("long", c(5, 10, 15)),
      name = "fill value",
      guide = guide_legend(
        direction = "horizontal",
        theme = theme(
          legend.title.position = "top",
          legend.text.position = "bottom",
          legend.title = element_text(size = 11, angle = 180, hjust = 0, vjust = 1),
          legend.text = element_text(size = 0.8 * 11, angle = 90, hjust = 1, vjust = 0.5)
        ),
        order = 1
      )
    )

  expect_doppelganger("rotated guide titles and labels", p )

  # title justification
  p <- ggplot(data.frame(x = 1:2)) +
    aes(x, x, colour = factor(x), fill = factor(x), shape = factor(x), alpha = x) +
    geom_point() +
    scale_alpha(breaks = 1:2) +
    guides(
      colour = guide_legend(
        "colour title with hjust = 0", order = 1,
        theme = theme(legend.title = element_text(hjust = 0))
      ),
      fill   = guide_legend(
        "fill title with hjust = 1", order = 2,
        theme = theme(
          legend.title = element_text(hjust = 1),
          legend.title.position = "bottom"
        ),
        override.aes = list(shape = 21)
      ),
      alpha  = guide_legend(
        "Title\nfor\nalpha\nwith\nvjust=0", order = 3,
        theme = theme(
          legend.title = element_text(vjust = 0),
          legend.title.position = "left"
        )
      ),
      shape = guide_legend(
        "Title\nfor\nshape\nwith\nvjust=1", order = 4,
        theme = theme(
          legend.title = element_text(vjust = 1),
          legend.title.position = "right"
        )
      )
    )
  expect_doppelganger("legends with all title justifications", p)
})

test_that("bin guide can be styled correctly", {
  df <- data_frame(x = c(1, 2, 3),
                   y = c(6, 5, 7))

  p <- ggplot(df, aes(x, y, size = x)) +
    geom_point() +
    scale_size_binned()

  expect_doppelganger("guide_bins looks as it should", p)
  expect_doppelganger("guide_bins can show limits",
    p + guides(size = guide_bins(show.limits = TRUE))
  )
  expect_doppelganger("guide_bins can show arrows",
    p + guides(size = guide_bins()) +
      theme_test() +
      theme(
        legend.axis.line = element_line(
          linewidth = 0.5 / .pt,
          arrow = arrow(length = unit(1.5, "mm"), ends = "both")
        )
      )
  )
  expect_doppelganger("guide_bins can remove axis",
    p + guides(size = guide_bins()) +
      theme_test() +
      theme(
        legend.axis.line = element_blank()
      )
  )
  expect_doppelganger("guide_bins work horizontally",
    p + guides(size = guide_bins(direction = "horizontal"))
  )
})

test_that("coloursteps guide can be styled correctly", {
  df <- data_frame(x = c(1, 2, 4),
                   y = c(6, 5, 7))

  p <- ggplot(df, aes(x, y, colour = x)) +
    geom_point() +
    scale_colour_binned(breaks = c(1.5, 2, 3))

  expect_doppelganger("guide_coloursteps looks as it should", p)
  expect_doppelganger("guide_coloursteps can show limits",
    p + guides(colour = guide_coloursteps(show.limits = TRUE))
  )
  expect_doppelganger("guide_coloursteps can have bins relative to binsize",
    p + guides(colour = guide_coloursteps(even.steps = FALSE))
  )
  expect_doppelganger("guide_bins can show ticks and transparancy",
    p + guides(colour = guide_coloursteps(
      alpha = 0.75,
      theme = theme(legend.ticks = element_line(linewidth = 0.5 / .pt, colour = "white"))
    ))
  )
})

test_that("binning scales understand the different combinations of limits, breaks, labels, and show.limits", {
  p <- ggplot(mpg, aes(cty, hwy, color = year)) +
    geom_point()

  expect_doppelganger("guide_bins understands coinciding limits and bins",
    p + scale_color_binned(limits = c(1999, 2008),
                           breaks = c(1999, 2000, 2002, 2004, 2006),
                           guide = 'bins')
  )
  expect_doppelganger("guide_bins understands coinciding limits and bins 2",
    p + scale_color_binned(limits = c(1999, 2008),
                           breaks = c(2000, 2002, 2004, 2006, 2008),
                           guide = 'bins')
  )
  expect_doppelganger("guide_bins understands coinciding limits and bins 3",
    p + scale_color_binned(limits = c(1999, 2008),
                           breaks = c(1999, 2000, 2002, 2004, 2006),
                           guide = 'bins', show.limits = TRUE)
  )
  expect_doppelganger("guide_bins sets labels when limits is in breaks",
    p + scale_color_binned(limits = c(1999, 2008),
                           breaks = c(1999, 2000, 2002, 2004, 2006),
                           labels = 1:5, guide = 'bins')
  )
  expect_snapshot_warning(ggplotGrob(p + scale_color_binned(labels = 1:4, show.limits = TRUE, guide = "bins")))

  expect_doppelganger("guide_colorsteps understands coinciding limits and bins",
    p + scale_color_binned(limits = c(1999, 2008),
                           breaks = c(1999, 2000, 2002, 2004, 2006))
  )
  expect_doppelganger("guide_colorsteps understands coinciding limits and bins 2",
    p + scale_color_binned(limits = c(1999, 2008),
                           breaks = c(2000, 2002, 2004, 2006, 2008))
  )
  expect_doppelganger("guide_colorsteps understands coinciding limits and bins 3",
    p + scale_color_binned(limits = c(1999, 2008),
                           breaks = c(1999, 2000, 2002, 2004, 2006),
                           show.limits = TRUE)
  )
  expect_doppelganger("guide_colorsteps sets labels when limits is in breaks",
    p + scale_color_binned(limits = c(1999, 2008),
                           breaks = c(1999, 2000, 2002, 2004, 2006),
                           labels = 1:5)
  )
  expect_snapshot_warning(ggplotGrob(p + scale_color_binned(labels = 1:4, show.limits = TRUE)))
})

test_that("a warning is generated when guides(<scale> = FALSE) is specified", {
  df <- data_frame(x = c(1, 2, 4),
                   y = c(6, 5, 7))

  # warn on guide(<scale> = FALSE)
  lifecycle::expect_deprecated(g <- guides(colour = FALSE))
  expect_equal(g$guides[["colour"]], "none")

  # warn on scale_*(guide = FALSE)
  p <- ggplot(df, aes(x, y, colour = x)) + scale_colour_continuous(guide = FALSE)
  lifecycle::expect_deprecated(ggplot_build(p))
})

test_that("guides() warns if unnamed guides are provided", {
  expect_snapshot_warning(guides("axis"))
  expect_snapshot_warning(guides(x = "axis", "axis"))
  expect_null(guides())
})

test_that("old S3 guides can be implemented", {

  my_env <- env()
  my_env$guide_circle <- function() {
    structure(
      list(available_aes = c("x", "y"), position = "bottom"),
      class = c("guide", "circle")
    )
  }

  registerS3method(
    "guide_train", "circle",
    function(guide, ...) guide,
    envir = my_env
  )
  registerS3method(
    "guide_transform", "circle",
    function(guide, ...) guide,
    envir = my_env
  )
  registerS3method(
    "guide_merge", "circle",
    function(guide, ...) guide,
    envir = my_env
  )
  registerS3method(
    "guide_geom", "circle",
    function(guide, ...) guide,
    envir = my_env
  )
  registerS3method(
    "guide_gengrob", "circle",
    function(guide, ...) {
      absoluteGrob(
        gList(circleGrob()),
        height = unit(1, "cm"), width = unit(1, "cm")
      )
    },
    envir = my_env
  )

  withr::local_environment(my_env)

  expect_snapshot_warning(
    expect_doppelganger(
      "old S3 guide drawing a circle",
      ggplot(mtcars, aes(disp, mpg)) +
        geom_point() +
        guides(x = "circle")
    )
  )
})

test_that("guide_custom can be drawn and styled", {

  p <- ggplot() + guides(custom = guide_custom(
    circleGrob(r = unit(1, "cm")),
    title = "custom guide"
  ))

  expect_doppelganger(
    "stylised guide_custom",
    p + theme(legend.background = element_rect(fill = "grey50"),
              legend.title.position = "left",
              legend.title = element_text(angle = 90, hjust = 0.5))
  )

  expect_doppelganger(
    "guide_custom with void theme",
    p + theme_void()
  )
})

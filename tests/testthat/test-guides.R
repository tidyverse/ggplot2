skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("plotting does not induce state changes in guides", {

  guides <- guides(
    x      = guide_axis(title = "X-axis"),
    colour = guide_colourbar(title = "Colourbar"),
    shape  = guide_legend(title = "Legend"),
    size   = guide_bins(title = "Bins")
  )

  p <- ggplot(mpg, aes(displ, hwy, colour = cty, shape = factor(cyl),
                       size = cyl)) +
    geom_point() +
    guides

  snapshot <- serialize(as.list(p$guides), NULL)

  grob <- ggplotGrob(p)

  expect_identical(as.list(p$guides), unserialize(snapshot))
})

test_that("adding guides doesn't change plot state", {

  p1 <- ggplot(mtcars, aes(disp, mpg))

  expect_length(p1$guides$guides, 0)

  p2 <- p1 + guides(y = guide_axis(angle = 45))

  expect_length(p1$guides$guides, 0)
  expect_length(p2$guides$guides, 1)

  p3 <- p2 + guides(y = guide_axis(angle = 90))

  expect_length(p3$guides$guides, 1)
  expect_equal(p3$guides$guides[[1]]$params$angle, 90)
  expect_equal(p2$guides$guides[[1]]$params$angle, 45)
})

test_that("colourbar trains without labels", {
  g <- guide_colorbar()
  sc <- scale_colour_continuous(limits = c(0, 4), labels = NULL)

  out <- g$train(scale = sc)
  expect_equal(names(out$key), c("colour", ".value"))
})

test_that("Colorbar respects show.legend in layer", {
  df <- data_frame(x = 1:3, y = 1)
  p <- ggplot(df, aes(x = x, y = y, color = x)) +
    geom_point(size = 20, shape = 21, show.legend = FALSE)
  expect_false("guide-box" %in% ggplotGrob(p)$layout$name)
  p <- ggplot(df, aes(x = x, y = y, color = x)) +
    geom_point(size = 20, shape = 21, show.legend = TRUE)
  expect_true("guide-box" %in% ggplotGrob(p)$layout$name)
})

test_that("show.legend handles named vectors", {
  n_legends <- function(p) {
    g <- ggplotGrob(p)
    gb <- which(g$layout$name == "guide-box")
    if (length(gb) > 0) {
      n <- length(g$grobs[[gb]]) - 1
    } else {
      n <- 0
    }
    n
  }

  df <- data_frame(x = 1:3, y = 20:22)
  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20)
  expect_equal(n_legends(p), 2)

  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20, show.legend = c(color = FALSE))
  expect_equal(n_legends(p), 1)

  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20, show.legend = c(color = FALSE, shape = FALSE))
  expect_equal(n_legends(p), 0)

  # c.f.https://github.com/tidyverse/ggplot2/issues/3461
  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20, show.legend = c(shape = FALSE, color = TRUE))
  expect_equal(n_legends(p), 1)
})

test_that("axis_label_overlap_priority always returns the correct number of elements", {
  expect_identical(axis_label_priority(0), numeric(0))
  expect_setequal(axis_label_priority(1), seq_len(1))
  expect_setequal(axis_label_priority(5), seq_len(5))
  expect_setequal(axis_label_priority(10), seq_len(10))
  expect_setequal(axis_label_priority(100), seq_len(100))
})

test_that("a warning is generated when guides are drawn at a location that doesn't make sense", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    scale_y_continuous(guide = guide_axis(position = "top"))
  expect_warning(ggplot_build(plot), "Position guide is perpendicular")
})

test_that("a warning is not generated when a guide is specified with duplicate breaks", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    scale_y_continuous(breaks = c(20, 20))
  built <- expect_silent(ggplot_build(plot))
  expect_silent(ggplot_gtable(built))
})

test_that("a warning is generated when more than one position guide is drawn at a location", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    guides(
      y = guide_axis(position = "left"),
      y.sec = guide_axis(position = "left")
    )
  built <- expect_silent(ggplot_build(plot))

  expect_warning(ggplot_gtable(built), "Discarding guide")
})

test_that("a warning is not generated when properly changing the position of a guide_axis()", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    guides(
      y = guide_axis(position = "right")
    )
  built <- expect_silent(ggplot_build(plot))
  expect_silent(ggplot_gtable(built))
})

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

test_that("Using non-position guides for position scales results in an informative error", {
  p <- ggplot(mpg, aes(cty, hwy)) +
    geom_point() +
    scale_x_continuous(guide = guide_legend())
  expect_snapshot_warning(ggplot_build(p))
})

test_that("guide merging for guide_legend() works as expected", {

  merge_test_guides <- function(scale1, scale2) {
    scale1$guide <- guide_legend(direction = "vertical")
    scale2$guide <- guide_legend(direction = "vertical")
    scales <- scales_list()
    scales$add(scale1)
    scales$add(scale2)
    scales <- scales$scales

    aesthetics <- lapply(scales, `[[`, "aesthetics")
    scales <- rep.int(scales, lengths(aesthetics))
    aesthetics <- unlist(aesthetics, FALSE, FALSE)

    guides <- guides_list(NULL)
    guides <- guides$setup(scales, aesthetics)
    guides$train(scales, labs())
    guides$merge()
    guides$params
  }

  different_limits <- merge_test_guides(
    scale_colour_discrete(limits = c("a", "b", "c", "d")),
    scale_linetype_discrete(limits = c("a", "b", "c"))
  )
  expect_length(different_limits, 2)

  same_limits <- merge_test_guides(
    scale_colour_discrete(limits = c("a", "b", "c")),
    scale_linetype_discrete(limits = c("a", "b", "c"))
  )
  expect_length(same_limits, 1)
  expect_equal(same_limits[[1]]$key$.label, c("a", "b", "c"))

  same_labels_different_limits <- merge_test_guides(
    scale_colour_discrete(limits = c("a", "b", "c")),
    scale_linetype_discrete(limits = c("one", "two", "three"), labels = c("a", "b", "c"))
  )
  expect_length(same_labels_different_limits, 1)
  expect_equal(same_labels_different_limits[[1]]$key$.label, c("a", "b", "c"))

  same_labels_different_scale <- merge_test_guides(
    scale_colour_continuous(limits = c(0, 4), breaks = 1:3, labels = c("a", "b", "c")),
    scale_linetype_discrete(limits = c("a", "b", "c"))
  )
  expect_length(same_labels_different_scale, 1)
  expect_equal(same_labels_different_scale[[1]]$key$.label, c("a", "b", "c"))

  repeated_identical_labels <- merge_test_guides(
    scale_colour_discrete(limits = c("one", "two", "three"), labels = c("label1", "label1", "label2")),
    scale_linetype_discrete(limits = c("1", "2", "3"), labels = c("label1", "label1", "label2"))
  )
  expect_length(repeated_identical_labels, 1)
  expect_equal(repeated_identical_labels[[1]]$key$.label, c("label1", "label1", "label2"))
})

test_that("size = NA doesn't throw rendering errors", {
  df = data.frame(
    x = c(1, 2),
    group = c("a","b")
  )
  p <- ggplot(df, aes(x = x, y = 0, colour = group)) +
    geom_point(size = NA, na.rm = TRUE)

  expect_silent(plot(p))
})

test_that("guide specifications are properly checked", {
  expect_snapshot_error(validate_guide("test"))
  expect_snapshot_error(validate_guide(1))

  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, shape = factor(gear))) +
    guides(shape = "colourbar")

  expect_snapshot_warning(ggplotGrob(p))

  expect_snapshot_error(guide_legend(title.position = "leftish"))

  expect_snapshot_error(guide_colourbar()$transform())

  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = gear)) +
    guides(colour = guide_colourbar(label.position = "top"))
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = gear)) +
    guides(colour = guide_colourbar(direction = "horizontal", label.position = "left"))
  expect_snapshot_error(ggplotGrob(p))

  expect_snapshot_error(guide_legend(label.position = "test"))
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = gear)) +
    guides(colour = guide_legend(nrow = 2, ncol = 2))
  expect_snapshot_error(ggplotGrob(p))
})

test_that("colorsteps and bins checks the breaks format", {
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = paste("A", gear))) +
    guides(colour = "colorsteps")
  expect_snapshot_error(suppressWarnings(ggplotGrob(p)))
  p <- ggplot(mtcars) +
    geom_point(aes(mpg, disp, colour = paste("A", gear))) +
    guides(colour = "bins")
  expect_snapshot_error(suppressWarnings(ggplotGrob(p)))
})

test_that("legend reverse argument reverses the key", {

  scale <- scale_colour_discrete()
  scale$train(LETTERS[1:4])

  guides <- guides_list(NULL)
  guides <- guides$setup(list(scale), "colour")

  guides$params[[1]]$reverse <- FALSE
  guides$train(list(scale), labels = labs())
  fwd <- guides$get_params(1)$key

  guides$params[[1]]$reverse <- TRUE
  guides$train(list(scale), labels = labs())
  rev <- guides$get_params(1)$key

  expect_equal(fwd$colour, rev(rev$colour))
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
})


test_that("guide_colourbar merging preserves both aesthetics", {
  # See issue 5324

  scale1 <- scale_colour_viridis_c()
  scale1$train(c(0, 2))

  scale2 <- scale_fill_viridis_c()
  scale2$train(c(0, 2))

  g <- guide_colourbar()
  p <- g$params

  p1 <- g$train(p, scale1, "colour")
  p2 <- g$train(p, scale2, "fill")

  merged <- g$merge(p1, g, p2)

  expect_true(all(c("colour", "fill") %in% names(merged$params$key)))
})

test_that("guide_colourbar warns about discrete scales", {

  g <- guide_colourbar()
  s <- scale_colour_discrete()
  s$train(LETTERS[1:3])

  expect_warning(g <- g$train(g$params, s, "colour"), "needs continuous scales")
  expect_null(g)

})

test_that("guide_axis_logticks calculates appropriate ticks", {

  test_scale <- function(trans = identity_trans(), limits = c(NA, NA)) {
    scale <- scale_x_continuous(trans = trans)
    scale$train(scale$transform(limits))
    view_scale_primary(scale)
  }

  train_guide <- function(guide, scale) {
    params <- guide$params
    params$position <- "bottom"
    guide$train(params, scale, "x")
  }

  guide <- guide_axis_logticks(negative_small = 10)
  outcome <- c((1:10)*10, (2:10)*100)

  # Test the classic log10 transformation
  scale <- test_scale(log10_trans(), c(10, 1000))
  key <- train_guide(guide, scale)$logkey

  expect_equal(sort(key$x), log10(outcome))
  expect_equal(key$.type, rep(c(1,2,3), c(3, 2, 14)))

  # Test compound transformation
  scale <- test_scale(compose_trans(log10_trans(), reverse_trans()), c(10, 1000))
  key   <- train_guide(guide, scale)$logkey

  expect_equal(sort(key$x), -log10(rev(outcome)))

  # Test transformation with negatives
  scale <- test_scale(pseudo_log_trans(), c(-1000, 1000))
  key   <- train_guide(guide, scale)$logkey

  unlog <- sort(pseudo_log_trans()$inverse(key$x))
  expect_equal(unlog, c(-rev(outcome), 0, outcome))
  expect_equal(key$.type, rep(c(1,2,3), c(7, 4, 28)))

  # Test expanded argument
  scale <- test_scale(log10_trans(), c(20, 900))
  scale$continuous_range <- c(1, 3)

  guide <- guide_axis_logticks(expanded = TRUE)
  key   <- train_guide(guide, scale)$logkey

  expect_equal(sort(key$x), log10(outcome))

  guide <- guide_axis_logticks(expanded = FALSE)
  key   <- train_guide(guide, scale)$logkey

  expect_equal(sort(key$x), log10(outcome[-c(1, length(outcome))]))

  # Test with prescaled input
  guide <- guide_axis_logticks(prescale_base = 2)
  scale <- test_scale(limits = log2(c(10, 1000)))

  key <- train_guide(guide, scale)$logkey
  expect_equal(sort(key$x), log2(outcome))

  # Should warn when scale also has transformation
  scale <- test_scale(log10_trans(), limits = c(10, 1000))
  expect_snapshot_warning(train_guide(guide, scale)$logkey)
})

test_that("guide_legend uses key.spacing correctly", {
  p <- ggplot(mtcars, aes(disp, mpg, colour = factor(carb))) +
    geom_point() +
    guides(colour = guide_legend(
      ncol = 2, key.spacing.y = 1, key.spacing.x = 2
    ))

  expect_doppelganger("legend with widely spaced keys", p)
})

# Visual tests ------------------------------------------------------------

test_that("axis guides are drawn correctly", {
  theme_test_axis <- theme_test() + theme(axis.line = element_line(linewidth = 0.5))
  test_draw_axis <- function(n_breaks = 3,
                             break_positions = seq_len(n_breaks) / (n_breaks + 1),
                             labels = as.character,
                             positions = c("top", "right", "bottom", "left"),
                             theme = theme_test_axis,
                             ...) {

    break_labels <- labels(seq_along(break_positions))

    # create the axes
    axes <- lapply(positions, function(position) {
      draw_axis(break_positions, break_labels, axis_position = position, theme = theme, ...)
    })
    axes_grob <- gTree(children = do.call(gList, axes))

    # arrange them so there's some padding on each side
    gt <- gtable(
      widths = unit(c(0.05, 0.9, 0.05), "npc"),
      heights = unit(c(0.05, 0.9, 0.05), "npc")
    )
    gt <- gtable_add_grob(gt, list(axes_grob), 2, 2, clip = "off")
    plot(gt)
  }

  # basic
  expect_doppelganger("axis guides basic", function() test_draw_axis())
  expect_doppelganger("axis guides, zero breaks", function() test_draw_axis(n_breaks = 0))

  # overlapping text
  expect_doppelganger(
    "axis guides, check overlap",
    function() test_draw_axis(20, labels = function(b) comma(b * 1e9), check.overlap = TRUE)
  )

  # rotated text
  expect_doppelganger(
    "axis guides, zero rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = 0)
  )

  expect_doppelganger(
    "axis guides, positive rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = 45)
  )

  expect_doppelganger(
    "axis guides, negative rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = -45)
  )

  expect_doppelganger(
    "axis guides, vertical rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = 90)
  )

  expect_doppelganger(
    "axis guides, vertical negative rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = -90)
  )

  # dodged text
  expect_doppelganger(
    "axis guides, text dodged into rows/cols",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e9), n.dodge = 2)
  )
})

test_that("axis guides are drawn correctly in plots", {
  expect_doppelganger("align facet labels, facets horizontal",
    ggplot(mpg, aes(hwy, reorder(model, hwy))) +
      geom_point() +
      facet_grid(manufacturer ~ ., scales = "free", space = "free") +
      theme_test() +
      theme(strip.text.y = element_text(angle = 0))
  )
  expect_doppelganger("align facet labels, facets vertical",
    ggplot(mpg, aes(reorder(model, hwy), hwy)) +
      geom_point() +
      facet_grid(. ~ manufacturer, scales = "free", space = "free") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  )
  expect_doppelganger("thick axis lines",
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      theme_test() +
      theme(axis.line = element_line(linewidth = 5, lineend = "square"))
  )
})

test_that("axis guides can be customized", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    scale_y_continuous(
      sec.axis = dup_axis(guide = guide_axis(n.dodge = 2)),
      guide = guide_axis(n.dodge = 2)
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))

  expect_doppelganger("guide_axis() customization", plot)
})

test_that("guides can be specified in guides()", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    guides(
      x = guide_axis(n.dodge = 2),
      y = guide_axis(n.dodge = 2),
      x.sec = guide_axis(n.dodge = 2),
      y.sec = guide_axis(n.dodge = 2)
    )

  expect_doppelganger("guides specified in guides()", plot)
})

test_that("guides have the final say in x and y", {
  df <- data_frame(x = 1, y = 1)
  plot <- ggplot(df, aes(x, y)) +
    geom_point() +
    guides(
      x = guide_none(title = "x (primary)"),
      y = guide_none(title = "y (primary)"),
      x.sec = guide_none(title = "x (secondary)"),
      y.sec = guide_none(title = "y (secondary)")
    )

  expect_doppelganger("position guide titles", plot)
})

test_that("Axis titles won't be blown away by coord_*()", {
  df <- data_frame(x = 1, y = 1)
  plot <- ggplot(df, aes(x, y)) +
    geom_point() +
    guides(
      x = guide_axis(title = "x (primary)"),
      y = guide_axis(title = "y (primary)"),
      x.sec = guide_axis(title = "x (secondary)"),
      y.sec = guide_axis(title = "y (secondary)")
    )

  expect_doppelganger("guide titles with coord_trans()", plot + coord_trans())
  # TODO
  # expect_doppelganger("guide titles with coord_polar()", plot + coord_polar())
  # TODO
  # expect_doppelganger("guide titles with coord_sf()", plot + coord_sf())
})

test_that("guide_axis() draws minor ticks correctly", {
  p <- ggplot(mtcars, aes(wt, disp)) +
    geom_point() +
    theme(axis.ticks.length = unit(1, "cm"),
          axis.ticks.x.bottom = element_line(linetype = 2),
          axis.ticks.length.x.top = unit(-0.5, "cm"),
          axis.minor.ticks.x.bottom = element_line(colour = "red"),
          axis.minor.ticks.length.y.left = unit(-0.5, "cm"),
          axis.minor.ticks.length.x.top = unit(-0.5, "cm"),
          axis.minor.ticks.length.x.bottom = unit(0.75, "cm"),
          axis.minor.ticks.length.y.right = unit(5, "cm")) +
    scale_x_continuous(labels = label_math()) +
    guides(
      # Test for styling and style inheritance
      x = guide_axis(minor.ticks = TRUE),
      # # Test for opposed lengths
      y = guide_axis(minor.ticks = TRUE),
      # # Test for flipped lenghts
      x.sec = guide_axis(minor.ticks = TRUE),
      # # Test that minor.length doesn't influence spacing when no minor ticks are drawn
      y.sec = guide_axis(minor.ticks = FALSE)
    )
  expect_doppelganger("guides with minor ticks", p)
})

test_that("absent titles don't take up space", {

  p <- ggplot(mtcars, aes(disp, mpg, colour = factor(cyl))) +
    geom_point() +
    theme(
      legend.title = element_blank(),
      legend.margin = margin(),
      legend.position = "top",
      legend.justification = "left",
      legend.key = element_rect(colour = "black"),
      axis.line = element_line(colour = "black")
    )

  expect_doppelganger("left aligned legend key", p)
})

test_that("axis guides can be capped", {
  p <- ggplot(mtcars, aes(hp, disp)) +
    geom_point() +
    theme(axis.line = element_line()) +
    guides(
      x = guide_axis(cap = "both"),
      y = guide_axis(cap = "upper"),
      y.sec = guide_axis(cap = "lower"),
      x.sec = guide_axis(cap = "none")
    )
  expect_doppelganger("axis guides with capped ends", p)
})

test_that("logticks look as they should", {

  p <- ggplot(data.frame(x = c(-100, 100), y = c(10, 1000)), aes(x, y)) +
    geom_point() +
    scale_y_continuous(trans = compose_trans(log10_trans(), reverse_trans()),
                       expand = expansion(add = 0.5)) +
    scale_x_continuous(
      breaks = c(-100, -10, -1, 0, 1, 10, 100)
    ) +
    coord_trans(x = pseudo_log_trans()) +
    theme_test() +
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          axis.ticks.length.x.top = unit(-2.75, "pt")) +
    guides(
      x = guide_axis_logticks(
        title = "Pseudo-logticks with 1 as smallest tick",
        negative_small = 1
      ),
      y = guide_axis_logticks(
        title = "Inverted logticks with swapped tick lengths",
        long = 0.75, short = 2.25
      ),
      x.sec = guide_axis_logticks(
        negative_small = 0.1,
        title = "Negative length pseudo-logticks with 0.1 as smallest tick"
      ),
      y.sec = guide_axis_logticks(
        expanded = FALSE, cap = "both",
        title = "Capped and not-expanded inverted logticks"
      )
    )
  expect_doppelganger("logtick axes with customisation", p)

})

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

  # Placement of legend inside
  expect_doppelganger("legend inside plot, centered",
    p2 + theme(legend.position = c(.5, .5))
  )
  expect_doppelganger("legend inside plot, bottom left",
    p2 + theme(legend.justification = c(0,0), legend.position = c(0,0))
  )
  expect_doppelganger("legend inside plot, top right",
    p2 + theme(legend.justification = c(1,1), legend.position = c(1,1))
  )
  expect_doppelganger("legend inside plot, bottom left of legend at center",
    p2 + theme(legend.justification = c(0,0), legend.position = c(.5,.5))
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
        title.theme = element_text(size = 11, angle = 0, hjust = 0.5, vjust = 1),
        label.theme = element_text(size = 0.8*11, angle = 270, hjust = 0.5, vjust = 1),
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
        title.position = "top",
        label.position = "bottom",
        title.theme = element_text(size = 11, angle = 180, hjust = 0, vjust = 1),
        label.theme = element_text(size = 0.8*11, angle = 90, hjust = 1, vjust = 0.5),
        order = 1
      )
    )

  expect_doppelganger("rotated guide titles and labels", p )
})

test_that("size and linewidth affect key size", {
  df <- data_frame(x = c(0, 1, 2))
  p  <- ggplot(df, aes(x, x)) +
    geom_point(aes(size = x)) +
    geom_line(aes(linewidth = 2 - x)) +
    scale_size_continuous(range = c(1, 12)) +
    scale_linewidth_continuous(range = c(1, 20))

  expect_doppelganger("enlarged guides", p)
})

test_that("colorbar can be styled", {
  df <- data_frame(x = c(0, 1, 2))
  p <- ggplot(df, aes(x, x, color = x)) + geom_point()

  expect_doppelganger("white-to-red colorbar, white ticks, no frame",
    p + scale_color_gradient(low = 'white', high = 'red')
  )

  expect_doppelganger("white-to-red colorbar, thick black ticks, green frame",
    p + scale_color_gradient(
          low = 'white', high = 'red',
          guide = guide_colorbar(
            frame = element_rect(colour = "green"),
            frame.linewidth = 1.5 / .pt,
            ticks.colour = "black",
            ticks.linewidth = 2.5 / .pt
            )
        )
    )
})

test_that("guides can handle multiple aesthetics for one scale", {
  df <- data_frame(x = c(1, 2, 3),
                   y = c(6, 5, 7))

  p <- ggplot(df, aes(x, y, color = x, fill = y)) +
    geom_point(shape = 21, size = 3, stroke = 2) +
    scale_colour_viridis_c(
      name = "value",
      option = "B", aesthetics = c("colour", "fill")
    )

  expect_doppelganger("one combined colorbar for colour and fill aesthetics", p)
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
    p + guides(size = guide_bins(axis.arrow = arrow(length = unit(1.5, "mm"), ends = "both")))
  )
  expect_doppelganger("guide_bins can remove axis",
    p + guides(size = guide_bins(axis = FALSE))
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
  expect_doppelganger("guide_bins can show ticks",
    p + guides(colour = guide_coloursteps(ticks = TRUE))
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

test_that("guide_axis_theta sets relative angle", {

  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    scale_x_continuous(breaks = breaks_width(25)) +
    coord_radial(donut = 0.5) +
    guides(
      theta = guide_axis_theta(angle = 0, cap = "none"),
      theta.sec = guide_axis_theta(angle = 90, cap = "both")
    ) +
    theme(axis.line = element_line(colour = "black"))

  expect_doppelganger("guide_axis_theta with angle adapting to theta", p)
})

test_that("guide_axis_theta can be used in cartesian coordinates", {

  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    guides(x = "axis_theta", y = "axis_theta",
           x.sec = "axis_theta", y.sec = "axis_theta") +
    theme(
      axis.line.x.bottom = element_line(colour = "tomato"),
      axis.line.x.top    = element_line(colour = "limegreen"),
      axis.line.y.left   = element_line(colour = "dodgerblue"),
      axis.line.y.right  = element_line(colour = "orchid")
    )

  expect_doppelganger("guide_axis_theta in cartesian coordinates", p)
})

test_that("a warning is generated when guides(<scale> = FALSE) is specified", {
  df <- data_frame(x = c(1, 2, 4),
                   y = c(6, 5, 7))

  # warn on guide(<scale> = FALSE)
  expect_warning(g <- guides(colour = FALSE), "The `<scale>` argument of `guides()` cannot be `FALSE`. Use \"none\" instead as of ggplot2 3.3.4.", fixed = TRUE)
  expect_equal(g$guides[["colour"]], "none")

  # warn on scale_*(guide = FALSE)
  p <- ggplot(df, aes(x, y, colour = x)) + scale_colour_continuous(guide = FALSE)
  expect_snapshot_warning(ggplot_build(p))
})

test_that("guides() warns if unnamed guides are provided", {
  expect_warning(
    guides("axis"),
    "All guides are unnamed."
  )
  expect_warning(
    guides(x = "axis", "axis"),
    "The 2nd guide is unnamed"
  )
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
